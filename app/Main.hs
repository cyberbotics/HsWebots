{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Main where

import System.IO
import System.Environment
import Data.Generics
import Data.List
import Language.C
import Language.C.System.GCC
import Language.C.Data
import Language.C.Data.Ident
import Text.PrettyPrint
import Control.Monad
import Text.Shakespeare.Text
import qualified Data.Text.IO as T

whenM cond exp = if cond then exp else pure ()

main = do
  (filename:[]) <- getArgs
  parseMyFile filename >>= \i -> do
    case i of
      CTranslUnit v a -> do
        forM_ v $ \i -> do
          printFunc filename i

parseMyFile :: FilePath -> IO CTranslUnit
parseMyFile input_file =
  do parse_result <- parseCFile (newGCC "gcc") Nothing ["-Ideps/webots/include"] input_file
     case parse_result of
       Left parse_err -> error (show parse_err)
       Right ast      -> return ast

class CIdent a where
  toIdent :: a -> String

instance CIdent Ident where
 toIdent a = identToString a 

instance CIdent (CStorageSpecifier NodeInfo) where
 toIdent a = render $ pretty a 

instance CIdent (CTypeSpecifier NodeInfo) where
 toIdent a = render $ pretty a 

instance CIdent (CTypeQualifier NodeInfo) where
 toIdent a = render $ pretty a 

instance CIdent (CFunctionSpecifier NodeInfo) where
 toIdent a = render $ pretty a 

instance CIdent (CAlignmentSpecifier NodeInfo) where
 toIdent a = render $ pretty a 

instance CIdent (CDeclarationSpecifier NodeInfo) where
 toIdent a = render $ pretty a 

instance CIdent (CDeclaration NodeInfo) where
 toIdent (CDecl [CStorageSpec a] _ _) = toIdent a
 toIdent (CDecl [CTypeSpec a] _ _) = toIdent a
 toIdent (CDecl [CTypeQual a] _ _) = toIdent a
 toIdent (CDecl [CFunSpec a] _ _) = toIdent a
 toIdent (CDecl [CAlignSpec a] _ _) = toIdent a
 toIdent (CDecl a _ _) = intercalate "," $ map toIdent a
 toIdent a = "not-parsed"


csnd (CDecl [CStorageSpec a] b _) = b
csnd (CDecl [CTypeSpec a] b _) = b
csnd (CDecl [CTypeQual a] b _) = b
csnd (CDecl [CFunSpec a] b _) = b
csnd (CDecl [CAlignSpec a] b _) = b
csnd (CDecl a b _) = b
csnd a = undefined

getArgIds [] = []
getArgIds ((Just (CDeclr
                (Just arg)
                _
                _
                _
                _
              )):xs) = toIdent arg : getArgIds xs
getArgIds (_:xs) = "xxx" : getArgIds xs
  
printFunc :: FilePath -> CExternalDeclaration NodeInfo -> IO ()
printFunc path (CDeclExt (CDecl
                       rets
                       [(Just (CDeclr
                               (Just func_name)
                               (CFunDeclr (Right (args,_)) _ _:_)
                               _
                               _
                               _
                              )
                        ,_
                        ,_)]
                       (NodeInfo pos _ _)
                      )
          ) = whenM (path == posFile pos) $ do
  T.putStr [st|
#{toIdent func_name} :: #{argsH}IO #{retH} 
#{toIdent func_name} #{argIds} =
   #{bra} #{retC} { #{toIdent func_name}(#{argsC}) } #{ket}
|]          
  where
    bra = "[C.exp|"
    ket = "|]"
    retH = case map toIdent rets of
      ["void"] -> "()"
      ["char"] -> "CBool"
      ["int"] -> "CInt"
      ["double"] -> "CDouble"
      ["const","char"] -> "String"
      ["const","double"] -> "Ptr CDouble"
      [a] -> a
      _ -> "UnKnown"
    retC = case map toIdent rets of
      ["void"] -> "void"
      ["char"] -> "bool"
      ["int"] -> "int"
      ["double"] -> "double"
      ["const","char"] -> "const char*"
      ["const","double"] -> "const double*"
      [a] -> a
    argH arg = case arg of
      "void" -> "()"
      "char" -> "CBool"
      "int" -> "CInt"
      "double" -> "CDouble"
      "const,char" -> "String"
      "const,double" -> "Ptr CDouble"
      a -> a
      _ -> "UnKnown"
    argsH =
      case map (\(i :: CDeclaration NodeInfo) -> argH (toIdent i)) args of
        [] -> ""
        a -> (intercalate " -> " a) ++ " -> "
    argC arg = case arg of
      "void" -> "void"
      "char" -> "bool"
      "int" -> "int"
      "double" -> "double"
      "const,char" -> "const char*"
      "const,double" -> "const double*"
      a -> a
    args' = getArgIds (map (\(a,b,c) -> a) $ concat $ map (\a -> csnd a) args)
    argsC =
      case (map (\(i :: CDeclaration NodeInfo) -> argC (toIdent i)) args,args') of
        ([],_) -> ""
        (_,[]) -> ""
        (a,b) -> intercalate ", " $ map (\(aa,bb) -> "$("++ aa ++ " " ++ bb ++")" ) $ zip a b
    argIds = intercalate " " args'
printFunc path (CDeclExt a@(CDecl ret body (NodeInfo pos _ _))) = return ()
--  whenM (path == posFile pos) $ print a
printFunc path (CDeclExt a@(CStaticAssert exp lit _)) = return ()
printFunc path (CFDefExt a) = return ()
printFunc path (CAsmExt a _) = return ()


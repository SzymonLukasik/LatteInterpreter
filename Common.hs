module Common where

import AbsLatte

isDeclStmt :: AbsLatte.Stmt -> Bool
isDeclStmt (AbsLatte.Decl _ _) = True
isDeclStmt _ = False

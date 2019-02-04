module Runtime.Thread.LocalVariableTable(
    LocalVariableTable,
    Slot(..)
) where

type MaxVariableNum = Word

type LocalVariableTable = [Slot]
data Slot = NumType | ReferenceType String deriving (Show)


-- newLocalVariableTable :: MaxVariableNum -> LocalVariableTable
-- newLocalVariableTable 0 = []
-- newLocalVariableTable n = Slot
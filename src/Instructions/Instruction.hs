module Instructions.Instruction (
    newInstruction,
    executeInstruction
) where

import Data.Word
import Data.Int
import Data.Bits
import GHC.Float
import Instructions.BytecodeReader
import Runtime.Thread.JvmStackFrame
import Common
import Control.Monad.State.Lazy
import Data.Binary.IEEE754

type Const = Int32

checkZero :: (Num n,Eq n,Ord n) => n -> Bool
checkZero v
    | v == 0 = error "java.lang.ArithmeticException: 不可以除以0"
    | otherwise = True

-- todo
compareValue :: (Num n,Eq n,Ord n) => n -> n -> Int32
compareValue v1 v2
    | v1 > v2 = 1
    | otherwise = 0

newInstruction :: OpCode -> StateT BytecodeReader IO Instruction 
newInstruction op
    -- (Constants)
    | op == 0x00 = return Nop
    | op == 0x01 = return Aconst_null
    | op == 0x02 = return Iconst_m1
    | op == 0x03 = return Iconst_0
    | op == 0x04 = return Iconst_1
    | op == 0x05 = return Iconst_2
    | op == 0x06 = return Iconst_3
	| op == 0x07 = return Iconst_4
	| op == 0x08 = return Iconst_5
	| op == 0x09 = return Lconst_0
	| op == 0x0a = return Lconst_1
	| op == 0x0b = return Fconst_0
	| op == 0x0c = return Fconst_1
	| op == 0x0d = return Fconst_2
	| op == 0x0e = return Dconst_0
	| op == 0x0f = return Dconst_1
    | op == 0x10 = do
        -- read 1 byte
        v <- readInt8
        return $ Bipush v 
    | op == 0x11 = do
        -- read a short（2 bytes)
        v <- readInt16
        return $ Sipush v
	-- | op == 0x12 = Ldc        
	-- | op == 0x13 = Ldc_w      
    -- | op == 0x14 = Ldc2_w     
    
    -- (Loads)
    | op == 0x15 = do
        v <- readInt8
        return $ Iload (fromIntegral v :: Int)      
    | op == 0x16 = do
        v <- readInt8
        return $ Lload (fromIntegral v :: Int)      
    | op == 0x17 = do
        v <- readInt8
        return $ Fload (fromIntegral v :: Int)    
    | op == 0x18 = do
        v <- readInt8
        return $ Dload (fromIntegral v :: Int)        
    | op == 0x19 = do
        v <- readInt8
        return $ Aload (fromIntegral v :: Int)    
	| op == 0x1a = return Iload_0
	| op == 0x1b = return Iload_1
	| op == 0x1c = return Iload_2
	| op == 0x1d = return Iload_3
	| op == 0x1e = return Lload_0
	| op == 0x1f = return Lload_1
	| op == 0x20 = return Lload_2
	| op == 0x21 = return Lload_3
	| op == 0x22 = return Fload_0
	| op == 0x23 = return Fload_1
	| op == 0x24 = return Fload_2
	| op == 0x25 = return Fload_3
	| op == 0x26 = return Dload_0
	| op == 0x27 = return Dload_1
	| op == 0x28 = return Dload_2
	| op == 0x29 = return Dload_3
	| op == 0x2a = return Aload_0
	| op == 0x2b = return Aload_1
	| op == 0x2c = return Aload_2
	| op == 0x2d = return Aload_3
    -- | op == 0x2e = Iaload
	-- | op == 0x2f = Laload
	-- | op == 0x30 = Faload
	-- | op == 0x31 = Daload
    -- | op == 0x32 = Aaload
	-- | op == 0x33 = Baload
	-- | op == 0x34 = Caload
    -- | op == 0x35 = Saload
    
    -- (Stores)
    | op == 0x36 = do
        v <- readInt8
        return $ Istore (fromIntegral v :: Int)     
    | op == 0x37 = do
        v <- readInt8
        return $ Lstore (fromIntegral v :: Int) 
    | op == 0x38 = do
        v <- readInt8
        return $ Fstore (fromIntegral v :: Int) 
    | op == 0x39 = do
        v <- readInt8
        return $ Dstore (fromIntegral v :: Int) 
    | op == 0x3a = do
        v <- readInt8
        return $ Astore (fromIntegral v :: Int) 
	| op == 0x3b = return Istore_0
	| op == 0x3c = return Istore_1
	| op == 0x3d = return Istore_2
	| op == 0x3e = return Istore_3
	| op == 0x3f = return Lstore_0
	| op == 0x40 = return Lstore_1
	| op == 0x41 = return Lstore_2
	| op == 0x42 = return Lstore_3
	| op == 0x43 = return Fstore_0
	| op == 0x44 = return Fstore_1
	| op == 0x45 = return Fstore_2
	| op == 0x46 = return Fstore_3
	| op == 0x47 = return Dstore_0
	| op == 0x48 = return Dstore_1
	| op == 0x49 = return Dstore_2
	| op == 0x4a = return Dstore_3
	| op == 0x4b = return Astore_0
	| op == 0x4c = return Astore_1
	| op == 0x4d = return Astore_2
    | op == 0x4e = return Astore_3
	| op == 0x4f = return Iastore
	| op == 0x50 = return Lastore
	| op == 0x51 = return Fastore
	| op == 0x52 = return Dastore
	| op == 0x53 = return Aastore
	| op == 0x54 = return Bastore
	| op == 0x55 = return Castore
    | op == 0x56 = return Sastore
    
    -- (Stack)
	| op == 0x57 = return Pop
	| op == 0x58 = return Pop2
	| op == 0x59 = return Dup
	| op == 0x5a = return Dup_x1
	| op == 0x5b = return Dup_x2
	| op == 0x5c = return Dup2
	| op == 0x5d = return Dup2_x1
	| op == 0x5e = return Dup2_x2
    | op == 0x5f = return Swap
    -- (Math)
	| op == 0x60 = return Iadd
	| op == 0x61 = return Ladd
	| op == 0x62 = return Fadd
	| op == 0x63 = return Dadd
	| op == 0x64 = return Isub
	| op == 0x65 = return Lsub
	| op == 0x66 = return Fsub
	| op == 0x67 = return Dsub
	| op == 0x68 = return Imul
	| op == 0x69 = return Lmul
	| op == 0x6a = return Fmul
	| op == 0x6b = return Dmul
	| op == 0x6c = return Idiv
	| op == 0x6d = return Ldiv
    | op == 0x6e = return  Fdiv
    | op == 0x6f = return Ddiv
    | op == 0x70 = return Irem
    | op == 0x71 = return Lrem
    | op == 0x72 = return Frem
	| op == 0x73 = return Drem
	| op == 0x74 = return Ineg
	| op == 0x75 = return Lneg
	| op == 0x76 = return Fneg
	| op == 0x77 = return Dneg
	| op == 0x78 = return Ishl
	| op == 0x79 = return Lshl
	| op == 0x7a = return Ishr
	| op == 0x7b = return Lshr
	| op == 0x7c = return Iushr
	| op == 0x7d = return Lushr
	| op == 0x7e = return Iand
	| op == 0x7f = return Land
	| op == 0x80 = return Ior
	| op == 0x81 = return Lor
	| op == 0x82 = return Ixor
	| op == 0x83 = return Lxor
    | op == 0x84 = do
        v1 <- readUint8
        v2 <- readInt8
        let index = fromIntegral(fromIntegral v1 :: Word) :: Int
        let const = fromIntegral v2 :: Int32
        return $ Iinc index const
	| op == 0x85 = return I2l          -- (Conversions)
	| op == 0x86 = return I2f
	| op == 0x87 = return I2d
	| op == 0x88 = return L2i
	| op == 0x89 = return L2f
	| op == 0x8a = return L2d
	| op == 0x8b = return F2i
	| op == 0x8c = return F2l
	| op == 0x8d = return F2d
	| op == 0x8e = return D2i
	| op == 0x8f = return D2l
	| op == 0x90 = return D2f
	| op == 0x91 = return I2b
	| op == 0x92 = return I2c
    | op == 0x93 = return I2s
    -- (Comparisons)
	| op == 0x94 = return Lcmp
	| op == 0x95 = return Fcmpl
	| op == 0x96 = return Fcmpg
	| op == 0x97 = return Dcmpl
	| op == 0x98 = return Dcmpg
	| op == 0x99 = return Ifeq              --
	| op == 0x9a = return Ifne              --
	| op == 0x9b = return Iflt              --
	| op == 0x9c = return Ifge              --
	| op == 0x9d = return Ifgt              --
	| op == 0x9e = return Ifle              --
	| op == 0x9f = return If_icmpeq         --
	| op == 0xa0 = return If_icmpne         --
	| op == 0xa1 = return If_icmplt         --
	| op == 0xa2 = return If_icmpge         --
	| op == 0xa3 = return If_icmpgt         --
	| op == 0xa4 = return If_icmple         --
	| op == 0xa5 = return If_acmpeq         --
    | op == 0xa6 = return If_acmpne         --
    -- (Control)
    -- | op == 0xa7 = return Goto              -- todo
	-- | op == 0xa8 = Jsr            --
    -- | op == 0xa9 = Ret            --
	-- | op == 0xaa = return Table_switch      -- todo
    -- | op == 0xab = return Lookup_switch     -- todo
	-- | op == 0xac = Ireturn
    -- | op == 0xad = Lreturn
	-- | op == 0xae = Freturn
	-- | op == 0xaf = Dreturn
    -- | op == 0xb0 = Areturn
    -- | op == 0xb1 = Return
    
    -- (References)
	-- | op == 0xb2 = Get_static         --
	-- | op == 0xb3 = Put_static         --
	-- | op == 0xb4 = Get_field          --
    -- | op == 0xb5 = Put_field          --
	-- | op == 0xb6 = Invoke_virtual     --
	-- | op == 0xb7 = Invoke_special     --
	-- | op == 0xb8 = Invoke_static      --
	-- | op == 0xb9 = Invoke_interface   --
	-- | op == 0xba = Invoke_dynamic     --
	-- | op == 0xbb = New                --
	-- | op == 0xbc = New_array          --
	-- | op == 0xbd = Anew_array         --
	-- | op == 0xbe = Arraylength
	-- | op == 0xbf = Athrow
    -- | op == 0xc0 = Check_cast         --
	-- | op == 0xc1 = Instance_of        --
	-- | op == 0xc2 = Monitorenter
	-- | op == 0xc3 = Monitorexit
    
    -- (Extended)
    -- | op == 0xc4 = Wide                  -- todo
	-- | op == 0xc5 = Multi_anew_array   
	-- | op == 0xc6 = Ifnull                -- todo
	-- | op == 0xc7 = Ifnonnull             -- todo
    -- | op == 0xc8 = Goto_w                -- todo 
    -- | op == 0xc9 = Jsr_w              
 
    -- (Reservet)
	-- | op == 0xca = Breakpoint
	-- | op == 0xfe = Impdep1
	-- | op == 0xff = Impdep2
    | otherwise = error $ "UnSupport OpCode: " ++ show op

executeInstruction :: Instruction -> StateT JvmStackFrame IO ()
executeInstruction inst =
    case inst of
        Nop              -> return ()
        Aconst_null      -> pushReference NULL
        Iconst_m1        -> pushInt (-1)
        Iconst_0         -> pushInt 0
        Iconst_1         -> pushInt 1
        Iconst_2         -> pushInt 2
        Iconst_3         -> pushInt 3
        Iconst_4         -> pushInt 4
        Iconst_5         -> pushInt 5
        Lconst_0         -> pushLong 1
        Lconst_1         -> pushLong 1
        Fconst_0         -> pushFloat 0.0
        Fconst_1         -> pushFloat 1.0
        Fconst_2         -> pushFloat 2.0
        Dconst_0         -> pushDouble 0.0
        Dconst_1         -> pushDouble 1.0
        Bipush b         -> pushInt $ (fromIntegral b :: Int32)
        Sipush s         -> pushInt $ (fromIntegral s :: Int32)
	    -- Ldc              -> return ()
	    -- Ldc_w            -> return ()
        -- Ldc2_w           -> return ()
        Iload i          -> do               -- ^ (Loads) Load @int@ from local variable
           v <- getInt i
           pushInt v     
        Lload i          -> do               -- ^ Load @long@ from local variable
           v <- getLong i
           pushLong v   
        Fload i          -> do               -- ^ Load @float@ from local variable
           v <- getFloat i
           pushFloat v
        Dload i          -> do               -- ^ Load @double@ from local variable   
            v <- getDouble i
            pushDouble v
        Aload i          -> do               -- ^ Load @reference@ from local variable
            v <- getReference i
            pushReference v
        Iload_0         -> do
            v <- getInt 0
            pushInt v
        Iload_1         -> do
            v <- getInt 1
            pushInt v
        Iload_2         -> do
            v <- getInt 2
            pushInt v
        Iload_3         -> do
            v <- getInt 3
            pushInt v
        Lload_0         -> do
            v <- getLong 0
            pushLong v 
        Lload_1         -> do
            v <- getLong 1
            pushLong v 
        Lload_2         -> do
            v <- getLong 2
            pushLong v 
        Lload_3         -> do
            v <- getLong 3
            pushLong v 
        Fload_0         -> do
            v <- getFloat 0
            pushFloat v
        Fload_1         -> do
            v <- getFloat 1
            pushFloat v
        Fload_2         -> do
            v <- getFloat 2
            pushFloat v
        Fload_3         -> do
            v <- getFloat 3
            pushFloat v
        Dload_0         -> do
            v <- getDouble 0
            pushDouble v
        Dload_1         -> do
            v <- getDouble 1
            pushDouble v
        Dload_2         -> do
            v <- getDouble 2
            pushDouble v
        Dload_3         -> do
            v <- getDouble 3
            pushDouble v
        Aload_0         -> do
            v <- getReference 0
            pushReference v
        Aload_1         -> do
            v <- getReference 1
            pushReference v
        Aload_2         -> do
            v <- getReference 2
            pushReference v
        Aload_3         -> do
            v <- getReference 3
            pushReference v
    --  Iaload         -> return ()
	--  Laload         -> return ()
	--  Faload         -> return ()
	--  Daload         -> return ()
    --  Aaload         -> return ()
	--  Baload         -> return ()
	--  Caload         -> return ()
    --  Saload         -> return ()
        Istore i       -> do            -- ^ (Stores) Store @int@ into local variable
            v <- popInt
            setInt i v
        Lstore i       -> do            -- ^ Store @long@ into local variable
            v <- popLong
            setLong i v
        Fstore i       -> do            -- ^ Store @float@ into local variable
            v <- popFloat
            setFloat i v
        Dstore i       -> do            -- ^ Store @double@ into local variable
            v <- popDouble
            setDouble i v
        Astore i       -> do            -- ^ Store @reference@ into local variable
            v <- popReference
            setReference i v
        Istore_0       -> do
            v <- popInt
            setInt 0 v
        Istore_1       -> do
            v <- popInt
            setInt 1 v
        Istore_2       -> do
            v <- popInt
            setInt 2 v
        Istore_3       -> do
            v <- popInt
            setInt 3 v
        Lstore_0       -> do
            v <- popLong
            setLong 0 v
        Lstore_1       -> do
            v <- popLong
            setLong 1 v
        Lstore_2       -> do
            v <- popLong
            setLong 2 v
        Lstore_3       -> do
            v <- popLong
            setLong 3 v
        Fstore_0       -> do
            v <- popFloat
            setFloat 0 v
        Fstore_1       -> do
            v <- popFloat
            setFloat 1 v
        Fstore_2       -> do
            v <- popFloat
            setFloat 2 v
        Fstore_3       -> do
            v <- popFloat
            setFloat 3 v
        Dstore_0       -> do
            v <- popDouble
            setDouble 0 v
        Dstore_1       -> do
            v <- popDouble
            setDouble 1 v
        Dstore_2       -> do
            v <- popDouble
            setDouble 2 v
        Dstore_3       -> do
            v <- popDouble
            setDouble 3 v
        Astore_0       -> do
            v <- popReference
            setReference 0 v
        Astore_1       -> do
            v <- popReference
            setReference 1 v
        Astore_2       -> do
            v <- popReference
            setReference 2 v
        Astore_3       -> do
            v <- popReference
            setReference 3 v
        -- Iastore        -> do             -- ^ (TODO)  Store into @int@ array
        --     return ()
        -- Lastore        -> do             -- ^ Store into @long@ array
        --     return ()
        -- Fastore        -> do             -- ^ Store into @float@ array
        --     return ()
        -- Dastore        -> do
        --     return ()
        -- Aastore        -> do
        --     return ()
        -- Bastore        -> do
        --     return ()
        -- Castore        -> do
        --     return ()
        -- Sastore        -> do
        --     return ()
        Pop            -> do                -- ^ (Stack) Pop the top operand stack value
            popValue
            return ()           
        Pop2           -> do                -- ^ Pop the top one or two operand stack values
            popValue
            popValue
            return ()
        Dup            -> do                -- ^ Duplicate the top operand stack value
           v <- popValue
           pushValue v
           pushValue v
        Dup_x1         -> do                -- ^ Duplicate the top operand stack value and insert two values down
           t1 <- popValue
           t2 <- popValue
           pushValue t1
           pushValue t2
           pushValue t1
        Dup_x2         -> do                -- ^ Duplicate the top operand stack value and insert 2 or 3 values down
           t1 <- popValue
           t2 <- popValue
           t3 <- popValue
           pushValue t1
           pushValue t3
           pushValue t2
           pushValue t1
        Dup2           -> do                -- ^ Duplicate the top one or two operand stack values
           t1 <- popValue
           t2 <- popValue 
           pushValue t2
           pushValue t1
           pushValue t2
           pushValue t1
        Dup2_x1       -> do                -- ^ Duplicate the top 1 or 2 operand stack values and insert 2 or 3 values down
           t1 <- popValue
           t2 <- popValue
           t3 <- popValue
           pushValue t2
           pushValue t1
           pushValue t3
           pushValue t2
           pushValue t1 
        Dup2_x2       -> do                -- ^ Duplicate the top 1 or 2 operand stack values and insert 2, 3, or 4 values down
           t1 <- popValue
           t2 <- popValue
           t3 <- popValue
           t4 <- popValue
           pushValue t2
           pushValue t1
           pushValue t4
           pushValue t3
           pushValue t2
           pushValue t1 
        Swap          -> do              -- ^ Swap the top two operand stack values
           t1 <- popValue
           t2 <- popValue
           pushValue t1
           pushValue t2 
        Iadd          -> do              -- ^ (Math) Add @int@
            v1 <- popInt
            v2 <- popInt
            pushInt $ v1 + v2
        Ladd          -> do              -- ^ Add @long@ 
            v1 <- popLong
            v2 <- popLong
            pushLong $ v1 + v2
        Fadd          -> do
            v1 <- popFloat
            v2 <- popFloat
            pushFloat $ v1 + v2
        Dadd          -> do
            v1 <- popDouble
            v2 <- popDouble
            pushDouble $ v1 + v2
        Isub          -> do
            v1 <- popInt
            v2 <- popInt
            pushInt $ v2 - v1
        Lsub          -> do
            v1 <- popLong
            v2 <- popLong
            pushLong $ v2 - v1
        Fsub          -> do
            v1 <- popFloat
            v2 <- popFloat
            pushFloat $ v2 - v1
        Dsub          -> do
            v1 <- popDouble
            v2 <- popDouble
            pushDouble $ v2 - v1
        Imul          -> do
            v1 <- popInt
            v2 <- popInt
            pushInt $ v2 * v1
        Lmul          -> do
            v1 <- popLong
            v2 <- popLong
            pushLong $ v2 * v1
        Fmul          -> do
            v1 <- popFloat
            v2 <- popFloat
            pushFloat $ v2 * v1
        Dmul          -> do
            v1 <- popDouble
            v2 <- popDouble
            pushDouble $ v2 * v1
        Idiv          -> do
            v1 <- popInt
            v2 <- popInt
            guard $ checkZero v1
            pushInt $ v2 `div` v1
        Ldiv          -> do
            v1 <- popLong
            v2 <- popLong
            guard $ checkZero v1
            pushLong $ v2 `div` v1
        Fdiv          -> do
            v1 <- popFloat
            v2 <- popFloat
            guard $ checkZero v1
            let n = wordToFloat $ (floatToWord v2) `div` (floatToWord v1)
            pushFloat n
        Ddiv          -> do
            v1 <- popDouble
            v2 <- popDouble
            guard $ checkZero v1
            let n = wordToDouble $ (doubleToWord v2) `div` (doubleToWord v1)
            pushDouble n
        Irem          -> do
            v1 <- popInt
            v2 <- popInt
            guard $ checkZero v1
            pushInt $ v2 `mod` v1
        Lrem          -> do
            v1 <- popLong
            v2 <- popLong
            guard $ checkZero v1
            pushLong $ v2 `mod` v1
        Frem          -> do
            v1 <- popFloat
            v2 <- popFloat
            guard $ checkZero v1
            -- TODO: no test
            let w1 = doubleToWord $ float2Double v1

            let w2 = doubleToWord $ float2Double v2
            
            pushFloat . double2Float $ wordToDouble (w2 `mod` w1)
        Drem          -> do
            v1 <- popDouble
            v2 <- popDouble
            guard $ checkZero v1
            -- TODO: no test
            let w1 = doubleToWord v1
            let w2 = doubleToWord v2
            pushDouble $ wordToDouble (w2 `mod` w1)
        Ineg          -> do             -- ^ Negate @int@
            v <- popInt
            pushInt (-v)
        Lneg          -> do
            v <- popLong
            pushLong (-v)
        Fneg          -> do
            v <- popFloat
            pushFloat (-v)
        Dneg          -> do
            v <- popDouble
            pushDouble (-v)
        Ishl          -> do             -- ^ Shift left @int@
            v1 <- popInt
            v2 <- popInt
            -- 0x1f = 31
            let v = shiftL v2 (fromIntegral ((fromIntegral v1 :: Word32) .&. 0x1f) :: Int)
            pushInt (fromIntegral v :: Int32)
        Lshl          -> do             -- ^ Shift left @long@    
            v1 <- popInt
            v2 <- popLong
            -- 0x3f = 63
            let v = shiftL v2 (fromIntegral ((fromIntegral v1 :: Word32) .&. 0x3f) :: Int)
            pushLong v
        Ishr          -> do             -- ^ Arithmetic shift right @int@
            v1 <- popInt
            v2 <- popInt
            let v = shiftR v2 (fromIntegral ((fromIntegral v1 :: Word32) .&. 0x1f) :: Int)
            pushInt (fromIntegral v :: Int32)
        Lshr          -> do             -- ^ Arithmetic shift right @long@
            v1 <- popInt
            v2 <- popLong
            let v = shiftR v2 (fromIntegral ((fromIntegral v1 :: Word32) .&. 0x3f) :: Int)
            pushLong (fromIntegral v :: Int64)
        Iushr         -> do             -- ^ Logical shift right @int@
            v1 <- popInt
            v2 <- popInt
            let n = fromIntegral ((fromIntegral v1 :: Word32) .&. 0x1f) :: Int
            let v = (fromIntegral $ shiftR (fromIntegral v2 :: Word32) n) :: Int32
            pushInt v
        Lushr         -> do             -- ^ Logical shift right @long@  
            v1 <- popLong
            v2 <- popLong
            let n = fromIntegral ((fromIntegral v1 :: Word32) .&. 0x3f) :: Int
            let v = (fromIntegral $ shiftR (fromIntegral v2 :: Word64) n) :: Int64
            pushLong v
        Iand          -> do              -- ^ Boolean AND @int@
            v1 <- popInt
            v2 <- popInt
            pushInt $ (v2 .&. v1)
        Land          -> do              -- ^ Boolean AND @long@
            v1 <- popLong
            v2 <- popLong
            pushLong $ (v2 .&. v1)
        Ior           -> do              -- ^ Boolean OR @int@ 
            v1 <- popInt
            v2 <- popInt
            pushInt $ (v2 .|. v1) 
        Lor           -> do              -- ^ Boolean OR @long@
            v1 <- popLong
            v2 <- popLong
            pushLong $ (v2 .|. v1)
        Ixor          -> do              -- ^ Boolean XOR @int@
            v1 <- popInt
            v2 <- popInt
            pushInt $ (v1 `xor` v2) 
        Lxor          -> do              -- ^ Boolean XOR @long@
            v1 <- popLong
            v2 <- popLong
            pushLong $ (v1 `xor` v2)
        Iinc i c       -> do             -- ^ Increment local variable by constant
            v <- getInt i
            setInt i (v + c)
        I2l          -> do              -- ^ (Conversions) Convert @int@ to @long@
            v <- popInt
            let i64 = fromIntegral v :: Int64
            pushLong i64 
        I2f          -> do              -- ^ Convert @int@ to @float@
            v <- popInt
            let f = fromIntegral v :: Float
            pushFloat f 
        I2d          -> do              -- ^ Convert @int@ to @double@
            v <- popInt
            let d = fromIntegral v :: Double
            pushDouble d 
        L2i          -> do
            v1 <- popLong
            let v = fromIntegral v1 :: Int32
            pushInt v
        L2f          -> do
            v1 <- popLong
            let f = fromIntegral v1 :: Float
            pushFloat f
        L2d          -> do
            v1 <- popLong
            let d = fromIntegral v1 :: Double
            pushDouble d
        F2i          -> do
            f <- popFloat
            let w = floatToWord f
            let v = fromIntegral w :: Int32
            pushInt v
        F2l          -> do
            f <- popFloat
            let d = float2Double f
            let w = doubleToWord d 
            let l = fromIntegral w :: Int64
            pushLong l
        F2d          -> do
            f <- popFloat
            let d = float2Double f
            pushDouble d
        D2i          -> do
            d <- popDouble
            let i = fromIntegral (doubleToWord d) :: Int32
            pushInt i
        D2l          -> do
            d <- popDouble
            let w = doubleToWord d 
            let l = fromIntegral w :: Int64
            pushLong l
        D2f          -> do
            d <- popDouble
            let f = double2Float d
            pushFloat f
        I2b          -> do
            v1 <- popInt
            let bytes = fromIntegral (fromIntegral v1 :: Int8) :: Int32
            pushInt bytes
        I2c          -> do
            v1 <- popInt
            let char = fromIntegral (fromIntegral v1 :: Word16) :: Int32
            pushInt char
        I2s          -> do
            v1 <- popInt
            let short = fromIntegral (fromIntegral v1 :: Int16) :: Int32
            pushInt short
        Lcmp          -> do         -- ^ (Comparisons) Compare @long@
            v1 <- popLong
            v2 <- popLong
            let v = compareValue v2 v1
            pushInt v
	    -- Fcmpl          -> do
	    -- Fcmpg          -> do
	    -- Dcmpl          -> do
	    -- Dcmpg          -> do
	    -- Ifeq i          -> do
	    -- Ifne i          -> do
	    -- Iflt i          -> do
	    -- Ifge i          -> do
	    -- Ifgt i          -> do
	    -- Ifle i          -> do
	    -- If_icmpeq i          -> do
	    -- If_icmpne i          -> do
	    -- If_icmplt i          -> do
	    -- If_icmpge i          -> do
	    -- If_icmpgt i          -> do
	    -- If_icmple i          -> do
	    -- If_acmpeq i          -> do
        -- If_acmpne i          -> do
        -- Goto i           -> do           -- (Control)
	--  Jsr            -> return ()
    --  Ret            -> return ()
	    -- Table_switch i          -> do
        -- Lookup_switch i          -> do
	--  Ireturn       -> return ()
    --  Lreturn      -> return ()
	--  Freturn      -> return ()
	--  Dreturn      -> return ()
    --  Areturn      -> return ()
    --  Return       -> return ()
	--  Get_static           -> return ()     -- (References)
	--  Put_static           -> return ()
	--  Get_field            -> return ()
    --  Put_field            -> return ()
	--  Invoke_virtual       -> return ()
	--  Invoke_special       -> return ()
	--  Invoke_static        -> return ()
	--  Invoke_interface     -> return ()
	--  Invoke_dynamic       -> return ()
	--  New                  -> return ()
	--  New_array            -> return ()
	--  Anew_array           -> return ()
	--  Arraylength          -> return ()
	--  Athrow               -> return ()
    --  Check_cast           -> return ()
	--  Instance_of          -> return ()
	--  Monitorenter         -> return ()
	--  Monitorexit          -> return ()
        -- Wide                  --  ^(Extended)
	-- Multi_anew_array   -> return ()
        
        Ifnull                          -> return ()
        
        Ifnonnull                       -> return ()
        Goto_w                          -> return ()
    -- Jsr_w                 -> return ()
	-- Breakpoint   -> return ()   -- ^(Reservet)
	-- Impdep1      -> return ()
	-- Impdep2      -> return ()

data Instruction =
    -- (Constants)
    Nop
    | Aconst_null
    | Iconst_m1
    | Iconst_0
    | Iconst_1
    | Iconst_2
    | Iconst_3
	| Iconst_4
	| Iconst_5
	| Lconst_0
	| Lconst_1
	| Fconst_0
	| Fconst_1
	| Fconst_2
	| Dconst_0
	| Dconst_1
	| Bipush Int8       --
    | Sipush Int16       --
	-- | Ldc        --
	-- | Ldc_w      --
    -- | Ldc2_w     --
    
    -- (Loads)
	| Iload Index    --
	| Lload Index       --
	| Fload Index        --
	| Dload Index       --
	| Aload Index        --
	| Iload_0
	| Iload_1
	| Iload_2
	| Iload_3
	| Lload_0
	| Lload_1
	| Lload_2
	| Lload_3
	| Fload_0
	| Fload_1
	| Fload_2
	| Fload_3
	| Dload_0
	| Dload_1
	| Dload_2
	| Dload_3
	| Aload_0
	| Aload_1
	| Aload_2
	| Aload_3
    -- | Iaload
	-- | Laload
	-- | Faload
	-- | Daload
    -- | Aaload
	-- | Baload
	-- | Caload
    -- | Saload
    
    -- (Stores)
	| Istore Index       
	| Lstore Index 
	| Fstore Index      
	| Dstore Index       
	| Astore Index       
	| Istore_0
	| Istore_1
	| Istore_2
	| Istore_3
	| Lstore_0
	| Lstore_1
	| Lstore_2
	| Lstore_3
	| Fstore_0
	| Fstore_1
	| Fstore_2
	| Fstore_3
	| Dstore_0
	| Dstore_1
	| Dstore_2
	| Dstore_3
	| Astore_0
	| Astore_1
	| Astore_2
    | Astore_3
	| Iastore
	| Lastore
	| Fastore
	| Dastore
	| Aastore
	| Bastore
	| Castore
    | Sastore
    
    -- (Stack)
	| Pop
	| Pop2
	| Dup
	| Dup_x1
	| Dup_x2
	| Dup2
	| Dup2_x1
	| Dup2_x2
    | Swap
    

    -- (Math)
	| Iadd
	| Ladd
	| Fadd
	| Dadd
	| Isub
	| Lsub
	| Fsub
	| Dsub
	| Imul
	| Lmul
	| Fmul
	| Dmul
	| Idiv
	| Ldiv
	| Fdiv
	| Ddiv
	| Irem
	| Lrem
	| Frem
	| Drem
	| Ineg
	| Lneg
	| Fneg
	| Dneg
	| Ishl
	| Lshl
	| Ishr
	| Lshr
	| Iushr
	| Lushr
	| Iand
	| Land
	| Ior
	| Lor
	| Ixor
	| Lxor
    | Iinc Int Const         --
    

    -- (Conversions)
	| I2l
	| I2f
	| I2d
	| L2i
	| L2f
	| L2d
	| F2i
	| F2l
	| F2d
	| D2i
	| D2l
	| D2f
	| I2b
	| I2c
    | I2s
    
    -- (Comparisons)
	| Lcmp
	| Fcmpl
	| Fcmpg
	| Dcmpl
	| Dcmpg
	| Ifeq              --
	| Ifne              --
	| Iflt              --
	| Ifge              --
	| Ifgt              --
	| Ifle              --
	| If_icmpeq         --
	| If_icmpne         --
	| If_icmplt         --
	| If_icmpge         --
	| If_icmpgt         --
	| If_icmple         --
	| If_acmpeq         --
    | If_acmpne         --
    

    -- (Control)
    | Goto              --
	-- | Jsr            --
    -- | Ret            --
	| Table_switch      --
    | Lookup_switch     --
	-- | Ireturn
    -- | Lreturn
	-- | Freturn
	-- | Dreturn
    -- | Areturn
    -- | Return
    
    -- (References)
	-- | Get_static         --
	-- | Put_static         --
	-- | Get_field          --
    -- | Put_field          --
	-- | Invoke_virtual     --
	-- | Invoke_special     --
	-- | Invoke_static      --
	-- | Invoke_interface   --
	-- | Invoke_dynamic     --
	-- | New                --
	-- | New_array          --
	-- | Anew_array         --
	-- | Arraylength
	-- | Athrow
    -- | Check_cast         --
	-- | Instance_of        --
	-- | Monitorenter
	-- | Monitorexit
    
    -- (Extended)
    | Wide                  --
	-- | Multi_anew_array   --
	| Ifnull                --
	| Ifnonnull             --
    | Goto_w                --
    -- | Jsr_w              --
 
    -- (Reservet)
	-- | Breakpoint
	-- | Impdep1
    -- | Impdep2
    


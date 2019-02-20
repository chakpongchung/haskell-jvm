module Runtime.Heap.ClassLoader (

) where

import ClassPath.ClassPath
import qualified Data.Map.Strict as Map
import Runtime.Heap.JavaClass
import qualified Data.ByteString.Lazy as L

-- 类完全限定名
type ClassQualifiedName = String

-- ^ 方法区: [类完全限定名,Java类]
type MethodArea = Map.Map ClassQualifiedName JavaClass

-- 类加载器
data ClassLoader = ClassLoader {
    cp :: ClassPath,                            
    methodArea :: MethodArea 
}   

-- 创建类加载器
newClassLoader :: ClassPath -> ClassLoader
newClassLoader c = ClassLoader {
    cp = c,
    methodArea = Map.empty
}


loadClassToMethodArea ::  ClassQualifiedName -> MethodArea -> StateT IO ClassLoader
loadClassToMethodArea n m = do
--    Map.lookup n m <|> loadClass n
        c <- get
        classContent <- lift $ loadClass c n
        -- 解析class文件
        let classFile = runGet parserClass classContent
        -- 将加载好的类信息转换成JavaClass对象
        let JavaClass = newJavaClass classFile c

        modify (\o -> o {methodArea = Map.insert n java $ methodArea o})
        return ()
   where 
        -- 加载，这里直接使用ClassPath类中的readClass方法读取类
        loadClass :: ClassLoader -> ClassQualifiedName -> IO L.ByteString
        loadClass = readClass . cp

        -- 链接（校验/准备/解析）
        -- link 


        -- 初始化
        -- init


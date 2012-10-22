module VM where
import Data.Word(Word64, Word32)

-- Memory
type Word = Word32
data Memory = Memory [(Word, Word)]

--{-
readMemory :: Memory -> Word -> Word
readMemory (Memory memoryMap) address = if address == key then value else readMemory (Memory memoryMapLeft) address
  where
    (key, value):memoryMapLeft = memoryMap

writeMemory :: Memory -> (Word, Word) -> Memory
writeMemory (Memory memoryMap) (key, value) = if key == address then (Memory ((key, value):newMemoryMap)) else (Memory ((address, word):newMemoryMap))
  where
    (address, word):memoryMapLeft = memoryMap
    (Memory newMemoryMap) = writeMemory (Memory memoryMapLeft) (key, value)
---}
-- CPU
-- CPU needs to support multiple mode
-- CPU dispatches instructions and operate on Memory
type AcceptanceTest = Word -> Bool
type Instruction = Word -> ([(String, Word)], Memory) -> ([(String, Word)], Memory)

data CPU =  CPU [(AcceptanceTest, Instruction)] [(String, Word)] Memory
execute :: CPU -> Word -> CPU
execute (CPU instructions registers memory) word = CPU instructions newRegisters newMemory
  where
    (_, instruction) = head (takeWhile (\(test, _) -> test word) instructions)
    (newRegisters, newMemory) = instruction word (registers, memory)

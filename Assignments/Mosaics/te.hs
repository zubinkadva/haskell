main = interact processInput

processInput input = [line | line <- take 10 (lines input)]
# Book 1

Haskell.
In.
Small.
Pieces.

Because: why not?

## Chapter 1: Reading input; writing output. 


```Haskell
main :: IO ()
main = getLine >>= 
       putStrLn . ("Hello, " ++)
```

[src](/src/book/book1/ex1a-io-monad.hs)

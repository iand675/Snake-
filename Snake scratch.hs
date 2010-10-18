Goal: Design a functional design rule set for the C++ programming language that will allow programmers to implement pure, referentially transparent functions efficiently for production-quality code while maintaining an equivalent level of performance as compared to impure imperative code.

Specifically, criteria for success include:

Preventing the creation of copies of objects when passing arguments to stateless functions in C++ while still maintaining functional purity.

Implementing a small game and functional library to demonstrate the necessary programming conventions for writing efficient, functional-style C++.


data Point = Point 
data Dir = Left
		 | Right
		 | Up
		 | Down
		deriving (Eq)		

data Snake  = Head (a,a) Snake
			| Body (a,a) Snake
			| Tail (a,a)
			
moveSnake :: 
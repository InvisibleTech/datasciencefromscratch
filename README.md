# datasciencefromscratch
My Scala version of the Machine Learning code  developed working through the book: "Data Science from Scratch". 

##NOTE:
A lot of the code here is an experiment in using immutable collections to parallel code written 
in Python from the book: Data Science from Scratch

That book makes have use of List and Map, and uses them in a mutable fashion.  This code is not
an efficient or refined model.  In some cases it is extreme - like applyFriends which uses 
lazy evaluation and currying like patterns to allow me to get a friend before I even know who 
their friends are.  I am certain - due to lack of depth in Scala FP - there are better ways.

I will try to keep driving this FP, Immutable Approach as long as possible.  Or until I surrender
and follow along with Python creating non-FP, mutable Scala.  I am trying to learn more about ML
by implementing it.  So I am trying to have fun.


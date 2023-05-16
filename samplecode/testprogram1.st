{%
	B10915012
	This is a test program of function calling
%}

var arr :array 1..10 of bool
var num :real := 1233

function a(a:int, b:int, c:int):int
	a := b + c	
	result a
end a

function b(a:string):int
	var i :int
	for i : 1..10 
		get arr[i]
	end for
	result 123
end b

function c(a: array 1..10 of bool, b:real):int
	result 1231231
end c

put a(b("asdasd""asd"), 1, c(arr, num))

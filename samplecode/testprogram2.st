{% B10915012 test asdasd\q13asdk
	This is a test program %} 
var arr: array 0..10 of int
var i:int
function totalSum(arr: array 0..10 of int):int
	var sum :int
	for i: 0..10
		sum := sum+arr[i]
	end for
	result sum
end totalSum

procedure displayList(arr: array 0..10 of int)
	put "Array elements: "
	for i: 0..10
		put arr[i]
	end for
	put " "
end displayList

% main program
for i: 0..10
	get arr[i]
end for
displayList(arr)
put totalSum(arr)


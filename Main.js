function lenAux(x,y)
{
    var k = [];
    var w = (x == k);
    if(w){
        return y;
    }else{
        return lenAux(x.tail, y+1);
    }
}

function len (x){
    return lenAux(x,0);
}

function quicksort(l){
	var list = l.tail;
	var menor = [];
	var maior = [];
	if(l == []){
		return [];
	}else{
		while (list != []) {
			if(l.head <= list.head){
				menor.concat(list.head);
			}else{
				maior.concat(list.head);
			}
			list = list.tail;
		}
		var major = [l.head].concat(quicksort(maior))
		var result = quicksort(menor).concat(major);
		return result;
	}
}

var lista1 = [1,2,3];
var c = len(lista1);
var lista2 = [12,1,4,9,32,22,55,1,9,0,5]
var z = quicksort(lista2);
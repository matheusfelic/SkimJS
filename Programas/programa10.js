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

function conca (ls, xs){
	return ls.concat(xs);
}

function quicksort(l){
	if(l == []){
		return [];
	}else{
	var list = l.tail;
	var menor = [];
	var maior = [];
	var result = [];
	var arrayHead = [l.head];
		while (list != []) {
			if(l.head <= list.head){
				menor = conca(menor, arrayHead);
			}else{
				maior = conca(arrayHead, maior);
			}
			list = list.tail;
		}
		var major = conca(arrayHead, quicksort(maior));
		result = conca(quicksort(menor), major);
		return result;
	}
}

var lista1 = [12,1,4,9,32,22,55,1,9,0,5]
var z = quicksort(lista1);
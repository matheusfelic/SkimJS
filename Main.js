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
	var arrayEmpty = [];
	if (l == arrayEmpty){
		return arrayEmpty;
	}
    var arrayHead = l.head;
	var listHead = [arrayHead];
    var menor = [];
    var maior = [];
    var result = [];
    var list = l.tail;
    if(listHead != arrayEmpty){
        while (list != arrayEmpty) {
            if(l.head < list.head){
                menor = conca(menor, listHead);
            }else{
                maior = conca(listHead, maior);
            }
                list = list.tail;
        }
        var major = conca(listHead, quicksort(maior));
        result = conca(quicksort(menor), major);
    }
        return result;
}
 
var lista1 = [12,1,4,9,32,22,55,1,9,0,5]
var z = quicksort(lista1);
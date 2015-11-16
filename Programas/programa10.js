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

var lista1 = [1,2,3];
var c = len(lista1);
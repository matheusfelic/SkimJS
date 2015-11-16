var lista1 = [1,2,3];
var lista2 = [4,5,6];
var lista3 = [1,2,3];

if(lista1 == lista2){
	lista1 = concat(lista1,lista2);
}

if(lista1 == lista3){
	lista1 = concat(lista1,lista2);
}

if(lista1 != lista3){
	lista3 = concat(lista3,lista1);
}
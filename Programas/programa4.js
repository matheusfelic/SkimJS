function f (x){
	if(x==0){
		return 0;
	}else {
		return f(x-1);
	}
}

var x = f(5);
function f (x){
	if(x==0){
		return 0;
	}else {
		return f(x-1);
	}
	y = x;
}

var x = f(5);
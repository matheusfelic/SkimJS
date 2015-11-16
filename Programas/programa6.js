function x (i)
{
	if(i < 2)
	{
		return 1;
	}
	else
	{
		var ret1 = x(i - 1) + x(i - 2);
		return ret1; 
	}
}

x(5);
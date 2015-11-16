var x = 0;

function rola()
{
	x = x + 1;
	if(x < 3)
	{
		rola();
	}
}

rola();


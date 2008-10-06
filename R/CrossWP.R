`CrossWP` <-
function (xwdS, ywdS) 
{
lx <- nlevels(xwdS)
ly <- nlevels(ywdS)

answdS <- xwdS

if (xwdS$type != ywdS$type)
	stop("X and Y wd objects are not both of type station")

if (lx != ly)
	stop("X and Y series do not have same number of levels")

else	{
	for(i in 0:(lx-1))	{
		dx <- accessD(xwdS, level=i)
		dy <- accessD(ywdS, level=i)

		answdS <- putD(answdS, level=i, v=dx*dy)
		}
	}
answdS
}


subroutine  xsucc(ksc,i,j,nadj,madj,ntot,nerror)

# Find the successor of j in the adjacency list of i.
# Called by addpt, initad, trifnd, swap, delout, dirseg, dirout.

implicit double precision(a-h,o-z)
dimension nadj(-3:ntot,0:madj)
dimension junk(20)

nerror = -1

n = nadj(i,0)

# If the adjacency list of i is empty, then clearly j has no successor
# in this adjacency list.  Something's wrong; stop.
if(n==0) {
	nerror = 9
	return
}

# The adjacency list of i is non-empty; search through it until j is found;
# add 1 to the location of j, and find the contents of this new location.
do k = 1,n {
    junk(k) = nadj(i,k)
}
call intpr("i =",-1,i,1)
call intpr("adj. list of i:",-1,junk,n)
do k = 1,n {
        if(j==nadj(i,k)) {
                kp = k+1
                if(kp>n) kp = 1         # Take kp modulo n. (The adjacency list
                ksc = nadj(i,kp)        # is circular.)
call intpr("k =",-1,k,1)
call intpr("kp =",-1,kp,1)
call intpr("ksc =",-1,ksc,1)
call intpr("junk(k) =",-1,junk(k),1)
call intpr("junk(kp) =",-1,junk(kp),1)
                return
        }
}

# The adjacency list doesn't contain j.  Something's wrong.
nerror = 10
return
end

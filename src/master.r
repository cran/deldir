subroutine master(x,y,sort,rw,npd,ntot,nadj,madj,ind,tx,ty,ilst,eps,
                  delsgs,ndel,delsum,dirsgs,ndir,dirsum,nerror)

# Master subroutine, to organize all the others.

implicit double precision(a-h,o-z)
logical sort
dimension x(-3:ntot), y(-3:ntot)
dimension nadj(-3:ntot,0:madj)
dimension ind(npd), tx(npd), ty(npd), ilst(npd), rw(4)
dimension delsgs(6,ndel), dirsgs(8,ndir)
dimension delsum(npd,4), dirsum(npd,3)

# Define one.
one = 1.d0

# Sort the points into bins, the number of such being approx. sqrt(n).
if(sort) {
	call binsrt(x,y,ntot,rw,npd,ind,tx,ty,ilst,nerror)
	if(nerror > 0) return
}
else {
	do i = 1,npd {
		ind(i) = i
	}
}

# Initialize the adjacency list to 0.
do i = -3,ntot {
	do j = 0,madj {
		nadj(i,j) = 0
	}
}

# Put the four ideal points into x and y and the adjacency list.
# The ideal points are given pseudo-coordinates
# (-1,-1), (1,-1), (1,1), and (-1,1).  They are numbered as
#    0       -1      -2         -3
# i.e. the numbers decrease anticlockwise from the
# `bottom left corner'.
x(-3) = -one
y(-3) =  one
x(-2) =  one
y(-2) =  one
x(-1) =  one
y(-1) = -one
x(0)  = -one
y(0)  = -one

do i = 1,4 {
        j = i-4
        k = j+1
        if(k>0) k = -3
	call insrt(j,k,nadj,madj,x,y,ntot,nerror,eps)
        if(nerror>0) return
}

# Put in the first of the point set into the adjacency list.
do i = 1,4 {
        j = i-4
	call insrt(1,j,nadj,madj,x,y,ntot,nerror,eps)
        if(nerror>0) return
}

# Now add the rest of the point set
do j = 2,npd {
	call addpt(j,nadj,madj,x,y,ntot,eps,nerror)
        if(nerror>0) return
}

# Obtain the description of the triangulation.
call delseg(delsgs,ndel,nadj,madj,x,y,ntot,ind,nerror)
if(nerror>0) return

call delout(delsum,nadj,madj,x,y,ntot,npd,ind,nerror)
if(nerror>0) return

call dirseg(dirsgs,ndir,nadj,madj,x,y,ntot,rw,eps,ind,nerror)
if(nerror>0) return

call dirout(dirsum,nadj,madj,x,y,ntot,npd,rw,ind,eps,nerror)

return
end

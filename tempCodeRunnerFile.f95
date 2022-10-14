program metropolis 

implicit none
integer :: i, rodadas = 50000, times_flipped = 0, x, y, vE, spin_i, spin_f,flips = 0
integer, dimension(100,100) :: rede, rede_flip
real :: r 
rede(1:100, 1:100) = 1
rede(10,10) = -1
print*, dE(1,1,rede)



contains 

integer function magnetizacao(lattice) result(m) !magnetização 
    implicit none 
    integer, dimension(100,100) :: lattice 
    integer ::  i, j

    m = 0
    do i = 1, 100
        do j = 1, 100
            m = m + lattice(i,j)
        end do 
    end do
end function magnetizacao

integer function dE(x,y,lattice) result(vE) !variacao de energia na rede depois de ter flipado o spin na coordenada x,y
    implicit none 
    integer, dimension (100, 100) :: lattice   
    integer :: x, y, N = 100

    vE = 0
    if ((x == N) .and. (y == N)) then
        vE = vE + lattice(x,y) *2* (lattice(1,y) + lattice(x-1,y) + lattice(x, 1) + lattice (x, y-1))
    else if (x == N) then
        vE = vE + lattice(x,y) *2* (lattice(1, y) + lattice(x-1,y) + lattice(x,y+1) + lattice(x,y-1))
    else if (y == N) then 
        vE = vE +lattice(x,y) *2* (lattice (x+1, y) + lattice(x-1,y) + lattice(x,1) + lattice(x,y-1))
    else  
        vE = vE +lattice(x,y) *2*(lattice(x+1, y) + lattice(x-1,y) + lattice(x,y+1) + lattice(x,y-1))
    end if 

end function dE
end program metropolis 
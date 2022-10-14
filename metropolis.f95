program metropolis 

implicit none
integer :: i, rodadas = 1000000, x, y, spin_i, spin_f, flip_times = 0, energia, vE
integer, dimension(100,100) :: rede, rede_flipada 
real :: r 

rede(1:100,1:100) = 1
!Metropolis

energia = energiaTotal(rede)

do i = 1, rodadas
    rede_flipada = rede  

    call random_number(r)
    x = int(r*100)
    call random_number(r)
    y = int(r*100)
!x, y coordenadas aleatorias do spin flipado
    spin_i = rede_flipada(x,y)
    spin_f = -1* spin_i

    rede_flipada(x,y) = spin_f 

    vE = dE(x,y,rede_flipada)

    call random_number(r)

    
!condicao para flipar 
    if (vE < 0 .or. exp(-real(vE)) > r) then
        
        rede(x,y) = spin_f
        flip_times = flip_times + 1
        
    end if 
end do 
print*, "rodadas ", rodadas
print*, "flip times ", flip_times
print*, "total energy", energiaTotal(rede)
print*, "magnetization", magnetizacao(rede)
 
   

contains

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

integer function energiaTotal(lattice) result(E)
    implicit none 
    integer, dimension(100,100) :: lattice
    integer :: x, y
    
    E = 0

    do x = 1, 100
        do y = 1, 100
            if (y /= 100) then 
                E = E - lattice(x,y)*lattice(x, y+1)
            else if (y == 100) then
                E = E - lattice(x,y)*lattice(x,1)
            end if 

            if (x /= 100) then 
                E = E - lattice(x,y) * lattice(x+1,y)
            else if (x == 100) then 
                E = E - lattice(x,y)*lattice(1,y)
            end if
        end do 
    end do 
    end function energiaTotal


end program metropolis
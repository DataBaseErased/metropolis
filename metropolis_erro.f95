program metropolis 

implicit none
integer, dimension(100,100) :: rede 

rede(1:100, 1:100) = 1

!Metropolis


   

contains

function metropolis(lattice)

    implicit none 

    integer :: rodadas = 1000, flip_times = 0
    integer :: t, x, y, spin_f, vE
    integer, dimension(100,100) :: lattice, lattice_flip 
    real :: r 
    lattice_flip= lattice
    do t = 1, rodadas
        lattice_flip = lattice 
        call random_number(r)
        x = int(r*100) 
        call random_number(r)
        y = int(r*100)

        spin_f = -1* lattice_flip(x,y)

        vE = dE(x,y, rede_flipada) 

        lattice_flip(x,y) = spin_f

        call random_number(r)

        if (vE < 0 .or. exp(-real(vE))> r) then 
            lattice(x,y) = spin_f 
            flip_times = flip_times + 1
        end if 
    end do 
end function metropolis


integer function dE(x,y,lattice) result(vE) !variacao de energia na rede depois de ter flipado o spin na coordenada x,y
    implicit none 
    integer, dimension (100, 100) :: lattice   
    integer :: x, y, N = 100

    vE = 0
    if ((x == N) .and. (y == N)) then
        vE = vE - lattice(x,y) *2* (lattice(1,y) + lattice(x-1,y) + lattice(x, 1) + lattice (x, y-1))
    else if (x == N) then
        vE = vE -lattice(x,y) *2* (lattice(1, y) + lattice(x-1,y) + lattice(x,y+1) + lattice(x,y-1))
    else if (y == N) then 
        vE = vE -lattice(x,y) *2* (lattice (x+1, y) + lattice(x-1,y) + lattice(x,1) + lattice(x,y-1))
    else  
        vE = vE -lattice(x,y) *2*(lattice(x+1, y) + lattice(x-1,y) + lattice(x,y+1) + lattice(x,y-1))
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
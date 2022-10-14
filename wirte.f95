program testando

integer :: i 

do i = 1, 3
    open (10, file = 'total_energy.txt')
    write (10,*) i
end do 
end program testando 
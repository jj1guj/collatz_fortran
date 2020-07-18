!10^4: 12ms
!10^5: 28ms
!10^6: 252ms
!10^7: 2889ms
program main
    implicit none
    integer(8) n,i,acc
    read*,n
    acc=0
    do i=1,n
        acc=acc+collatz(i)
        acc=mod(acc,1000000007)
    end do
    print'(i0)',acc
contains
integer(8) function collatz(n_in)
    integer(8) n,n_in
    collatz=0
    n=n_in

    do while(n/=1)
        collatz=collatz+1
        if(mod(n,2)==0)then
            n=n/2
        else
            n=3*n+1
        end if
    end do
end function
end program main
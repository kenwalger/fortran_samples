program tsunami

    use iso_fortran_env, only: int32, real32, output_unit

    implicit none

    integer(kind=int32) :: i, n ! indicies in space and time
    integer(kind=int32), parameter :: im = 100 ! grid size in space
    integer(kind=int32), parameter :: nm = 100 ! number of time steps

    real(kind=real32), parameter :: dt = 1 ! time step [s]
    real(kind=real32), parameter :: dx = 1 ! grid spacing [m]
    real(kind=real32), parameter :: c = 1 ! phase sppec [m/s]

    real(kind=real32), dimension(im) :: du, u

    integer(kind=int32), parameter :: ipos = 25
    real(kind=real32), parameter :: decay = 0.02

    ! initialize a gaussain blob centered at i = 25
    do i = 1, im
        u(i) = exp(-decay * (i - ipos)**2)
    end do

    write(unit=output_unit, fmt=*) 0, u

    time_loop: do n = 1, nm
        du(1) = u(1) - u(im)

        do concurrent (i = 2:im)
            du(i) = u(i) - u(i-1)
        end do

        do concurrent (i = 1:im)
            u(i) = u(i) - c * du(i) / dx * dt
        end do

        write(unit=output_unit, fmt=*) n, u

    end do time_loop

end program tsunami
program GameOfLife
    use mpi
    implicit none
    integer ierr, myid, numprocs
    integer send, recv, count, tag
    parameter (tag=111)
    integer stat(MPI_STATUS_SIZE)
    integer N, m, i, j, k, sum
    parameter (N=10) !# of squares per a processors
    integer W, H
    parameter (W=2,H=2) !# of processors up and across
    integer A(N,N), buff(0:N+1,0:N+1), G(N*H, N*W)
    integer recv_buff(N), send_buff(N), request 
    integer sizes(2), subsizes(2), starts(2), recvcount(W*H)
    integer newtype, intsize, resizedtype
    integer(kind=MPI_ADDRESS_KIND) extent, begin
    integer disp(W*H)
    double precision t1, t2
    real r
    
    call MPI_INIT(ierr)
    call MPI_COMM_RANK(MPI_COMM_WORLD, myid, ierr)
    call MPI_COMM_SIZE(MPI_COMM_WORLD, numprocs, ierr)
    
    ! fill up subgrid with random values
    ! ****************************
    ! do i = 1, N
    !     do j = 1, N
    !        call random_number(r)
    !        A(i,j) = floor(r*2) 
    !     end do
    ! end do
    ! ****************************

    ! glider for testing
    ! **********************
    do i = 1, N
         do j = 1, N
            A(i,j) = 0
         end do
    end do
    if ( myid == 0 ) then
        A(1,3) = 1
        A(2,3) = 1 
        A(3,3) = 1
        A(3,2) = 1
        A(2,1) = 1
    endif
    ! **********************

    ! t1 = MPI_Wtime()

    ! beginning of game of life grid iterations 
    do m = 1, 80 !1000000
        call MPI_Barrier(MPI_COMM_WORLD, ierr)
        buff(1:N,1:N) = A

        !passing bottom
        send_buff = A(N,:)
        call MPI_ISEND(send_buff, N, MPI_INTEGER, modulo(myid+1,H)+(myid/H)*H, tag, MPI_COMM_WORLD, request, ierr)
        call MPI_IRECV(recv_buff, N, MPI_INTEGER, modulo(myid-1,H)+(myid/H)*H, tag, MPI_COMM_WORLD, request, ierr)
        call MPI_WAIT(request, stat, ierr)
        buff(0,1:N) = recv_buff

        !passing top
        send_buff = A(1,:) 
        call MPI_ISEND(send_buff, N, MPI_INTEGER, modulo(myid-1,H)+(myid/H)*H, tag, MPI_COMM_WORLD, request, ierr)
        call MPI_IRECV(recv_buff, N, MPI_INTEGER, modulo(myid+1,H)+(myid/H)*H, tag, MPI_COMM_WORLD, request, ierr)
        call MPI_WAIT(request, stat, ierr)
        buff(N+1,1:N) = recv_buff 

        !passing left side
        send_buff = A(:,1)  
        call MPI_ISEND(send_buff, N, MPI_INTEGER, modulo(myid-H,numprocs), tag, MPI_COMM_WORLD, request, ierr)
        call MPI_IRECV(recv_buff, N, MPI_INTEGER, modulo(myid+H,numprocs), tag, MPI_COMM_WORLD, request, ierr)
        call MPI_WAIT(request, stat, ierr)
        buff(1:N,N+1) = recv_buff

        !passing right side 
        send_buff = A(:,N)
        call MPI_ISEND(send_buff, N, MPI_INTEGER, modulo(myid+H,numprocs), tag, MPI_COMM_WORLD, request, ierr)
        call MPI_IRECV(recv_buff, N, MPI_INTEGER, modulo(myid-H,numprocs), tag, MPI_COMM_WORLD, request, ierr)
        call MPI_WAIT(request, stat, ierr)
        buff(1:N,0) = recv_buff

        !passing upper right corner
        send_buff = A(1,N) 
        call MPI_ISEND(send_buff, 1, MPI_INTEGER, modulo(modulo(myid-1,H)+(myid/H)*H+H,numprocs), &
                        tag, MPI_COMM_WORLD, request, ierr)
        call MPI_IRECV(recv_buff, 1, MPI_INTEGER, modulo(modulo(myid+1,H)+(myid/H)*H-H,numprocs), &
                        tag, MPI_COMM_WORLD, request, ierr)
        call MPI_WAIT(request, stat, ierr)
        buff(N+1,0) = recv_buff(1)

        !passing upper left corner
        send_buff = A(1,1) 
        call MPI_ISEND(send_buff, 1, MPI_INTEGER, modulo(modulo(myid-1,H)+(myid/H)*H-H,numprocs), &
                        tag, MPI_COMM_WORLD, request, ierr)
        call MPI_IRECV(recv_buff, 1, MPI_INTEGER, modulo(modulo(myid+1,H)+(myid/H)*H+H,numprocs), &
                        tag, MPI_COMM_WORLD, request, ierr)
        call MPI_WAIT(request, stat, ierr)
        buff(N+1,N+1) = recv_buff(1)

        !passing lower left corner
        send_buff = A(N,1) 
        call MPI_ISEND(send_buff, 1, MPI_INTEGER, modulo(modulo(myid+1,H)+(myid/H)*H-H,numprocs), &
                        tag, MPI_COMM_WORLD, request, ierr)
        call MPI_IRECV(recv_buff, 1, MPI_INTEGER, modulo(modulo(myid-1,H)+(myid/H)*H+H,numprocs), &
                        tag, MPI_COMM_WORLD, request, ierr)
        call MPI_WAIT(request, stat, ierr)
        buff(0,N+1) = recv_buff(1)

        !passing lower right corner
        send_buff = A(N,N) 
        call MPI_ISEND(send_buff, 1, MPI_INTEGER, modulo(modulo(myid+1,H)+(myid/H)*H+H,numprocs), &
                        tag, MPI_COMM_WORLD, request, ierr)
        call MPI_IRECV(recv_buff, 1, MPI_INTEGER, modulo(modulo(myid-1,H)+(myid/H)*H-H,numprocs), &
                        tag, MPI_COMM_WORLD, request, ierr)
        call MPI_WAIT(request, stat, ierr)
        buff(0,0) = recv_buff(1)

        call MPI_Barrier(MPI_COMM_WORLD, ierr)

        ! do i = 0, N+1
        !     print *, buff(i, :)
        ! end do
        ! print *, myid

        ! do i = 1, N
        !     print *, A(i, :)
        ! end do

        do i = 1, N
            do j = 1, N
                sum = 0
                sum = sum + buff(i-1,j-1)
                sum = sum + buff(i,j-1)
                sum = sum + buff(i+1,j-1)
                sum = sum + buff(i+1,j)
                sum = sum + buff(i+1,j+1)
                sum = sum + buff(i,j+1)
                sum = sum + buff(i-1,j+1)
                sum = sum + buff(i-1,j)
                if ( sum == 3 ) then
                    A(i,j) = 1
                elseif ( sum == 2 ) then
                    A(i,j) = buff(i,j)
                else
                    A(i,j) = 0
                endif
            end do
        end do
    end do

    ! t2 = MPI_Wtime()
    ! print *, "Time Taken -->", t2 - t1, "seconds"

    starts   = [0,0]
    sizes    = [N*H, N*W]
    subsizes = [N, N]

    call MPI_Type_create_subarray(2, sizes, subsizes, starts, &
                                MPI_ORDER_FORTRAN, MPI_INTEGER, newtype, ierr)
    call MPI_Type_size(MPI_INTEGER, intsize, ierr)

    extent = intsize*N
    begin  = 0

    call MPI_Type_create_resized(newtype, begin, extent, resizedtype, ierr)
    call MPI_Type_commit(resizedtype, ierr)

    if ( myid == 0 ) then
        k = 1
        do i = 0, W-1
            do j = 0, H-1
                disp(k) = i*H*N + j
                k = k + 1
            end do
        end do
    endif

    recvcount = 1

    call MPI_GATHERV(A,N*N,MPI_INTEGER,G,recvcount,disp,resizedtype,0,MPI_COMM_WORLD,ierr)
    
    if ( myid == 0 ) then
        do i = 1, N*H
            print *, G(i,:)
        end do
    endif

    call MPI_Type_free(newtype,ierr)
    call MPI_Type_free(resizedtype,ierr)

    call MPI_FINALIZE(ierr)

end program GameOfLife

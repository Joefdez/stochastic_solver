
  subroutine platen_realization(YY, BB, neqs, dt, dst, nsteps, nsample_steps, final_steps)
    implicit none
    ! Calculates a realization of the stochastic process using the weak Platen scheme
    real(kind=8), intent(inout), dimension(:)   :: YY
    real(kind=8), dimension(:,:), intent(in)    :: BB
    integer, intent(in)                         :: neqs
    real(kind=8), intent(in)                    :: dt, dst, nsteps, nsample_steps, final_steps

    integer                                     :: ii, jj, kk
    real(kind=8), dimension(1:neqs)             :: YYi, AA, AAi, dOm

    !allocate(YYi(1:neqs))
    !allocate(AA(1:neqs))
    !allocate(AAi(1:neqs))
    !allocate(dOm(1:neqs))
    !allocate(BB(1:neqs, 1:neqs))

    do ii=1, nsteps-1, 1
      call deterministic_terms(YY(ii), AA)
      call stochastic_step(dOm, dst)
      YYi = YY(ii) + AA*dt + BB*dOm
      call deterministic_terms(YYi, AAi)
      YY(ii+1) = YY(ii) + 0.5d0*(AA+AAi)*dt + BB*dOm
    end do

  end subroutine platen_realization

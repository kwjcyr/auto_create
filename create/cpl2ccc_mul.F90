         !----------------------------------------------------
         ! cpl -> {ccc}
         !----------------------------------------------------	

         if (iamin_CPL{CCC}ID .and. {ccc}_prognostic) then
            call t_drvstartf ('DRIVER_C2{C}',barrier=mpicom_CPL{CCC}ID)
            
!<list> [segment1] [ccc1] [c1]
!           if({segment1}) then
!               call t_drvstartf ('driver_c2{c1}_{ccc1}x2{ccc1}{c1}',barrier=mpicom_CPL{ccc1}ID)
!               call map_{ccc1}x2{ccc1}{c1}_mct( cdata_{c1}x, x2{c1}_{c1}x, cdata_{c1}{c1}, x2{c1}_{c1}{c1})
!               call t_drvstopf  ('driver_c2{c1}_{ccc1}x2{ccc1}{c1}')
!           endif
!</list>

!<list> [segment3] [ccc3] [c3]
!           if ({segment3}) then
!               call t_drvstartf ('driver_c2{c3}_infoexch',barrier=mpicom_CPL{ccc3}ID)
!               call seq_infodata_exchange(infodata,CPL{ccc3}ID,'cpl2{ccc3}_run')
!               call t_drvstopf  ('driver_c2{c3}_infoexch')
!           endif
!</list>
            call t_drvstopf  ('DRIVER_C2{C}')
         endif

         !----------------------------------------------------
         ! cpl -> {ccc}
         !----------------------------------------------------	

         if (iamin_CPL{CCC}ID .and. {ccc}_prognostic) then
            call t_drvstartf ('DRIVER_C2{C}',barrier=mpicom_CPL{CCC}ID)
            call t_drvstartf ('driver_c2{c}_{ccc}x2{ccc}{c}',barrier=mpicom_CPL{CCC}ID)
            call map_{ccc}x2{ccc}{c}_mct( cdata_{c}x, x2{c}_{c}x, cdata_{c}{c}, x2{c}_{c}{c})
            call t_drvstopf  ('driver_c2{c}_{ccc}x2{ccc}{c}')
            call t_drvstartf ('driver_c2{c}_infoexch',barrier=mpicom_CPL{CCC}ID)
            call seq_infodata_exchange(infodata,CPL{CCC}ID,'cpl2{ccc}_run')
            call t_drvstopf  ('driver_c2{c}_infoexch')
            call t_drvstopf  ('DRIVER_C2{C}')
         endif

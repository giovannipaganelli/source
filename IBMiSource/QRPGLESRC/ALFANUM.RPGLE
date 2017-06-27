     H/TITLE Associazione matricola a fascetta cravatta
     H decedit('0,') datedit(*ymd/)
     H bnddir('MODSNSILE')
     H option(*srcstmt:*nodebugio)
     H optimize(*NONE)
     *---------------------------------------------------------------*
     *           E R M E N E G I L D O   Z E G N A                   *
     *                                                               *
     * Azienda       : CONSITEX SA                                   *
     * Programma     : ALFANUM                                       *
     * Dddcrizione   : Prova da char to numeric                      *
     *                                                               *
     *                                                               *
     *  (®) E.Z. Divisione Formale                                   *
     *  - Sistemi Informativi -                                      *
     *---------------------------------------------------------------*
      *INFDS DELLA DSPF:
     D WSFBA           DS
     D  @WSREC                38     45                                         Nome record
     D  @WSAID               369    369                                         Help attivato
     D  @WSCUR               370    371B 0                                      Posizione cursore
     D  @WSSFR               378    379B 0                                      N[reg subfile attivo
     *
    D PGMDS          SDS
     D  @pgm                   1     10
     D  @user                254    263
     D  @sds81                81    269
     D  @njob                264    269  0
     D  @parms           *parms
     D  @nsen                227    232  0
     D  @stat            *status
     D  @sds                   1    253
     *
     D num_val         s              6  0 inz(0)
     D num_val_c       s              6    inz
     D num_char        s              8    inz('55/44')
     D n_len           s              2  0 inz(0)
     D n_value         s              1  0 inz(0)
     D n_value_6       s              6  0 inz(0)
     D c_val           s              1    inz
     D x               s              2  0 inz
     D y               s              2  0 inz
     * ------------------------------------------------------------
     * --- M A I N
     * ------------------------------------------------------------
     *
      /free

            //ciclo per campo alfa e tolgo caratteri non numerici

                 n_len = %len(%trim(num_char));

           // INserito commento     fff
                for  x=1 to n_len ;
                     c_val = %subst(num_char:x:1) ;
                     //Verifico se numerico
                       monitor ;
                         N_value = %int(c_val) ;
                         y = y +1 ;
                         %subst(num_val_c:y:1) = %char(N_value);
                       on-error ;
                       endmon ;
                Endfor ;

            //Verifico che sia un numero
                   monitor ;
                     N_value_6 = %int(%trim(num_val_c)) ;
                   on-error ;
                     N_value_6 = 0 ;
                   endmon ;


       num_val = N_value_6 + 10;


         eval *inlr=*on ;
      /end-free
     * ----------------------------------------------

     H*BETD DECEDIT(*JOBRUN) DATEDIT(*DMY/) BNDDIR('MODSNSILE')
BETD H DECEDIT(*JOBRUN) DATEDIT(*YMD/) BNDDIR('MODSNSILE')
     H OPTION(*NODEBUGIO)
     *---------------------------------------------------------------*
     *           E R M E N E G I L D O   Z E G N A                   *
     *                                                               *
     * Azienda       : CONSITEX                                      *
     * Programma     : BF003L                                        *
     * Descrizione   : Driver aggiornamento archivi da CSIS a SNS    *
     * Data          : 26 Settembre 2006                             *
     * Programmatore : Marco Berizzi                                 *
     *                                                               *
     * Note          :                                               *
     *---------------------------------------------------------------*
     * Ballerini Ettore  14.11.2012 sigla BET5                       *
     * Corretta CPA per gestione PDC accoppiate su cicli nobili      *
 PG1 *---------------------------------------------------------------*
  "  * Descrizione   : Aggiunto controllo per variazione per         *
  "  *                 Assegnazione / disassegnazione in status BO   *
  "  * Data          : 07 Gennaio 2014                               *
  "  * Programmatore : Giovanni Paganelli                            *
 PG1 *                                                               *
BETD *---------------------------------------------------------------*
  "  * Descrizione   : Corrette specifihce H per uso con *DATE       *
  "  *                 Se non sapete come funziona il parametro      *
  "  *                 DATEDIT chiedete a qualcuno o guardatevi      *
  "  *                 il sorgente BET32 nella libreria ADAMOSRC     *
  "  * Data          : 16 Maggio 2014                                *
  "  * Programmatore : Ballerini Ettore                              *
BETD *                                                               *
BET3 *---------------------------------------------------------------*
  "  * Descrizione   : Messo controllo per capire che se vado in     *
  "  *                 FA su orcri, non devono esistere giacenze su  *
  "  *                 SNS . Vedi problema sollevato da Braccini in  *
  "  *                 data odierna                                  *
  "  * Data          : 23 Gennaio 2015                               *
  "  * Programmatore : Ballerini Ettore                              *
BET3 *                                                               *
     *                                                               *
     *---------------------------------------------------------------*
     *
     * File in INPUT
     FOVDRG51L  if   e           k disk
     FOADRG51L  if   e           k disk
     FBOVTE51L  if   e           k disk
     FBOVRG51L  if   e           k disk
     FSPVTE51L  if   e           k disk
     FBOVDA51L  if   e           k disk
     FLPRTE51L  if   e           k disk
     FLPRRG51L  if   e           k disk
     FBOVTE02L  if   e           k disk    RENAME(BOVTE:RBOVTE)
     FCTPPN01L  if   e           k disk
     FCONFA01L  if   e           k disk
     FUDBAN03K  if   e           k disk
     FEVEAN01K  if   e           k disk
     FMAGAN08K  if   e           k disk
     FMAGAN02K  if   e           k disk    RENAME(RMAGAN:MAG02)
     FSPVTE19L  if   e           k disk    RENAME(SPVTE:RSPVTE)
     FORTRI07K  if   e           k disk
     FMCOCC01L  if   e           k disk
     FUDBMF01K  if   e           k disk
     Fortrd06k  if   e           k disk
MMD  FSPATE01L  uf   e           k disk
MMD  Forcpa01k  uf a e           k disk    COMMIT
MMD  FFAATE01L  uf   e           k disk
MMD  FFRNAN01K  if   e           k disk
BET  Fmagan05k  if   e           k disk    RENAME(RMAGAN:MAG05)
BET  Ftsian99k  if   e           k disk
BET  FOVRRG02L  if   e           k disk
BET  FLPRRG06L  if   e           k disk    RENAME(LPRRG:RLPRRG) PREFIX($X)
BET9 FFAMAN01K  if   e           k disk
PG1  F**AVPRD18K  if   e           k disk
PG1  FAVPRD11K  if   e           k disk
BET9 FAVPRD03k  uf   e           k disk    Rename(ravprd: $avprd) prefix($)
BET9 F                                     commit
BET9 FAVPRD08k  if   e           k disk    Rename(ravprd: avprd08) prefix(X_)
BET9 FCPMAN01K  if   e           k disk
BET9 Ffapes01k  if   e           k disk
     *
     FSNS0102L  uf   e           k disk
     FSNS0101L  if   e           k disk    rename(sns01:rlass) prefix($rl)
     * File in aggiornamento sotto COMMIT
     FORCRI28K  uf   e           k disk    COMMIT
     FORCRI01K  uf   e           k disk    COMMIT RENAME(RORCRI:ORCRI01)
     FORCRI0AK  uf   e           k disk    COMMIT RENAME(RORCRI:ORCRI0A)
     FORCRI0BK  uf   e           k disk    COMMIT RENAME(RORCRI:ORCRI0B)
     FORCRI0CK  uf   e           k disk    COMMIT RENAME(RORCRI:ORCRI0C)
     FORCRI0DK  uf   e           k disk    COMMIT RENAME(RORCRI:ORCRI0D)
     FORCRI0MK  uf   e           k disk    COMMIT RENAME(RORCRI:ORCRI0M)
     F                                     PREFIX(M_)
     FS14TE01K  uf   e           k disk    COMMIT RENAME(RS14TE:S14TE01)
     FS14RI01K  uf   e           k disk    COMMIT RENAME(RS14RI:S14RI01)
     FSSCCB01K  uf   e           k disk    COMMIT RENAME(RSSCCB:SSCCB01)
     FORPRA55K  uf   e           k disk    COMMIT
     FSCACM01K  uf   e           k disk    COMMIT
     FSCACM02K  uf a e           k disk    COMMIT RENAME(RSCACM:SCA02)
     FS14TEGOF  o    e           k disk    COMMIT
     FS14RIGPF  o    e           k disk    COMMIT
     FSSCCBV5F  o    e           k disk    COMMIT
     FSNSSVFLF  o    e           k disk
     FWPFER02K  uf a e           k disk
     ****FERRVE01K  uf   e           k disk
BET  FORPRA01K  uf   e           k disk    COMMIT RENAME(RORPRA:ORPRA01)
     FOVTCM01L  uf   e           k disk    COMMIT
MO07 Ftotcm01l  uf   e           k disk    rename(totcm:rtotcm) prefix(z_)      UBOR+TDOR+PDOR+NDOR+NR
     FMGDIF01K  If a e           k disk    COMMIT
     FMPFAN01K  If   e           k disk
BET3 FMPFAN20K  If   e           k disk    rename(rmpfan:rmpfan20) prefix(a)
     Fcipro02k  if   e           k disk
     Fflufa01K  if   e           k disk
     Fcpmad01k  if   e           k disk
PG1  FLEGOR01k  uf   e           k disk    commit
PG1  FLegcm03k  if   e           k disk
PG1  FImbor10k  if   e           k disk
PG1  FBOARG51L  if   e           k disk
     *
     *****************************************************************
     D kpjba         e ds
     D ##udb         e ds                  inz
     D zbf004        e ds                  inz
     D zcl016        e ds                  inz
     D zmg039        e ds                  inz
     D zwl004        e ds                  inz
     D zmp708        e ds                  inz
     D zVe558        e ds                  inz
     D zbferr        e ds                  inz
     D z@cvar        e ds                  inz
     D z@ctpb        e ds                  inz
     D zbf006        e ds                  inz
     D zmp533        e ds                  inz
     D zbf042        e ds                  inz
     D zmp004        e ds                  inz
     D zsy921        e ds                  inz
MMD  D zmp810        e ds                  inz
MMD  D znumer        e ds                  inz
BET  D zve500        e ds                  inz
BET  D z@cmmg        e ds                  inz
BET  D zbf011        e ds                  inz
BET  D zlc911        e ds                  prefix($X)
BET  D zsn680        e ds                  prefix($_) inz
BET  D zsn679        e ds                  prefix($1) inz
BET  D $kpjbu          s                   like(kpjbu) inz
BET  D z@cfpd        e ds                  inz
BET9 D z@cawb        e ds                  inz
BET9 D zmg400        e ds                  inz prefix($40)
BET9 D z@tord        e ds                  inz
BET9 D zOR318        e ds                  inz prefix($OR)
BETA D zLN782        e ds                  inz prefix(x)
     D zmg125        e ds                  inz
     D z@ccpd        e ds                  inz
     D z@cblk        e ds                  inz
PG1  D ZOR184        e ds                  inz
PG1  D zp@gen        e ds                  inz

PG1  D DS_Box          ds                  inz
 "   D  Box_ser2               1      2
 "   D  Box_fil                3      3
 "   D  Box_ser3               1      3
 "   D  Nr_box                 4      8
 "    * --------
 "    * Key box
 "   D  xO5SBOX        s                   like(O5SBOX) inz
 "   D  xO5ANNB        s                   like(O5ANNB) inz
 "   D  xO5NUMB        s                   like(O5NUMB) inz
PG1  D  xO5NRGB        s                   like(O5NRGB) inz
     *
     D  CMD            S             80
     *
     D datayymd        S               D   datfmt(*iso)
     D datadmyy        S               D   datfmt(*eur)
     D dataiso         s               d   datfmt(*iso)
     *
     D $esegui         s               n   inz(*off)
     D $$ccpa          S                   like(adccpm)                         *las*
     D $$ccpp          S                   like(e6ccpp)                         *las*
     *
MMD  D                 DS
MMD  D  $$data                 1      8  0
MMD  D  $$anno                 1      4  0
MMD  D  $$mese                 5      6  0
MMD  D  $$gior                 7      8  0
     *
     D $cudf           s                   like(uttudb)
     D $cfor           s                   like(utcfor)
     D $tilav          s                   like(ctilib)
     D $ucfbp          s                   like(ibcfbp)
     D $ulfas          s                   like(ibfpdr)
     D $$kpjbu         s                   like(KPJBU) inz
BET  D $CVAR           S                   LIKE(MGCVAR)
BET  D $PROG           S                   LIKE(MGprog)
     D $dat06          s              6  0 inz
     D $dat08          s              8  0 inz
     D $$Dat8          s              8  0 inz
     D $data1          s              8  0 inz
     D $modo           s              2a   inz('  ')
     D $err_chk        s              1a
     D $errore         s              1a
     D $crearighe      s              1a
     D $annurighe      s              1a
     D $ccpa           s              1a
     D $qual           s              1a
     D $aggiocpa       s              1a   inz('N')
     D $iter           s              1a   inz('N')
     D $ctab           s              3a   inz('   ')
     D $cele           s              6a   inz('      ')
     D $statorcri      s              3a   inz('   ')
     D $X              s              2  0 inz
     D $Y              s              1  0 inz
     D $Z              s              1  0 inz
     D $cont           s              1  0 inz
     D $cont1          s              3  0 inz
     D sbn             s              1    dim(8)
     D sbx             s              1    dim(3)
     D nbo             s              1    dim(5)
     D $nufa           s              6a   inz('      ')
     D $nump           s              5a   inz('     ')
     D $numddt         s              5a   inz('     ')
     D $anno           s              4a   inz('    ')
     D $testata        s              1a   inz('Y')
     D $masm           s              2a   inz('  ')
     D $chiamaerr      s              1n   inz(*OFF)
     D $controllo      s              1n   inz(*OFF)
     D $doppione       s              1n   inz(*OFF)
     D $volta          s              1n   inz(*OFF)
     D $trovaspvte     s              1a   inz(' ')
     D $trovabovte     s              1a   inz(' ')
     D $trovabovrg     s              1a   inz(' ')
     D cpp             s              1    dim(5)
     D $trascotgl      s              1a   inz(' ')
     D $ttag           s              1a   inz(' ')
     D $udbpr          s              3a   inz('   ')
     D $udbde          s              3a   inz('   ')
PG1  D $assegna        s              1a   inz
     D $austat         s              1
PG1  D $CauAss         s                   like(CVARGF) inz ('PV')
PG1  D $CauAnn         s                   like(CVARGF) inz ('RV')
     D tgl01           s              3  0
     D tgl02           s              3  0
     D tgl03           s              3  0
     D tgl04           s              3  0
     D tgl05           s              3  0
     D tgl06           s              3  0
     D tgl07           s              3  0
     D tgl08           s              3  0
     D tgl09           s              3  0
     D tgl10           s              3  0
     D tgl11           s              3  0
     D tgl12           s              3  0
     D tgl13           s              3  0
     D tgl14           s              3  0
     D tgl15           s              3  0
     D tgl16           s              3  0
     D $$GGMMAAAA      s              8  0
     D*DATO            DS          9999
     D*DATN            DS          9999
     D $ubor           s                   like(hcubor) inz
     D $tdor           s                   like(hctdor) inz
     D $pdor           s                   like(hcpdor) inz
     D $ndor           s                   like(hcndor) inz
     D $nror           s                   like(hcnror) inz
     D $rigalis        s                   like(lrnrlp) inz
     D $Trovato        s               n   inz(*OFF)
BET5 D $PrimaCPP       S               n   inz(*OFF)
     D $bulk           S                   like(aucblk)
     D $agnon          S              1
     d
PG1  d $$CUDB          S                   like(AUCUDB) inz
 "   d $$CSTG          S                   like(AUCSTG) inz
 "   d $$CCLI          S                   like(AUCCLI) inz
 "   d $$NORD          S                   like(AUNORD) inz
 "   d $$NRIG          S                   like(AUNRIG) inz
 "   d $$SRIG          S                   like(AUSRIG) inz
 "    *
 "   d xS1UBOR         s                   like(S1UBOR) inz
 "   d xS1TDOR         s                   like(S1TDOR) inz
 "   d xS1PDOR         s                   like(S1PDOR) inz
 "   d xS1NDOR         s                   like(S1NDOR) inz
PG1  d xS1NROR         s                   like(S1NROR) inz
     *
BET  D $ptr$_qta       s               *   INZ(%ADDR($_QTA01))
BET  D $_Qta           s                   DIM(16) like($_qta01)
BET  D                                     based($ptr$_qta)
     *
BET  D $ptr$_qtaTG     s               *   INZ(%ADDR(RLN01))
BET  D RLN             s                   DIM(16) like(RLN01)
BET  D                                     based($ptr$_qtaTG)
     *
PG1   *-----------------------------------------------------------------
PG1  DLEGOR_DS       e ds                  extname(LEGORHMF)
PG1  D lPPCUDB       e                     extfld(PPCUDB)
PG1  D lPPCUTE       e                     extfld(PPCUTE)
PG1  D lPPFECH       e                     extfld(PPFECH)
PG1  D lPPHORA       e                     extfld(PPHORA)
PG1  D lPPNJOB       e                     extfld(PPNJOB)
PG1  D lPPNPGM       e                     extfld(PPNPGM)
BET9 DAVPRD03ds      e ds                  extname(AVPRDICF) prefix($)
BET9 DAVPRD00f       e ds                  extname(AVPRDICF)
BET9 D $ptr$_qtaAV     s               *   INZ(%ADDR(ICE01))
BET9 D ICE             s                   DIM(16) like(ICE01)
BET9 D                                     based($ptr$_qtaAV)
BET9 D $ptr$_qtaAV1    s               *   INZ(%ADDR(ICR01))
BET9 D ICR             s                   DIM(16) like(ICR01)
BET9 D                                     based($ptr$_qtaAV1)
BET9 D $ptr$_qtaAV2    s               *   INZ(%ADDR(ICZ01))
BET9 D ICZ             s                   DIM(16) like(ICZ01)
BET9 D                                     based($ptr$_qtaAV2)
BET9 D ICE$            s                   DIM(16) like(ICE01)
BET9 D ICR$            s                   DIM(16) like(ICR01)
BET9 D ICZ$            s                   DIM(16) like(ICZ01)
BET9 D LTQ$            s                   DIM(16) like(LTQ01)
BET9 D $TOTLTQ         s                   LIKE(S1TOQN)
BET9 D $CFPD           s                   LIKE(ICCFPD)
BET9 D $CFBP           s                   LIKE(ICCFBP)
BET9 D $GRPF           s                   LIKE(ICGRPF)
BET9 D $FCOL           s                   LIKE(FCOLIB)
BET9 D COLVIR          s                   LIKE(FCOLIB)
     D $sovraprod      s              1n   inz(*OFF)
     D ifnewa          s              1    inz
     D colc            s                   like(aucolc)
     D ctec            s                   like(auctec)
     D cmod            s                   like(aucmod)
     *
BET  D RLNS            s                   DIM(16) like(RLN01)
BET  D RLNC            s                   DIM(16) like(RLN01)
BET  D RLNSX           s                   DIM(16) like(RLN01)
BET9 D RLNS$           s                   DIM(16) like(RLN01)
BET9 D $QTATOTALE      s                   LIKE(S1TOQN)
PG1  D RLNX            s                   DIM(16) like(RLN01) inz
     *
BET2 D                 DS
BET6 D  e6q                    1     32P 0                                      Capi lanciati x
BET6 D                                     DIM(16)                              File ORPRA02K Ca
BET6 D  e6q01                  1      2P 0                                      Capi lanciati x
BET6 D  e6q02                  3      4P 0                                      Capi lanciati x
BET6 D  e6q03                  5      6P 0                                      Capi lanciati x
BET6 D  e6q04                  7      8P 0                                      Capi lanciati x
BET6 D  e6q05                  9     10P 0                                      Capi lanciati x
BET6 D  e6q06                 11     12P 0                                      Capi lanciati x
BET6 D  e6q07                 13     14P 0                                      Capi lanciati x
BET6 D  e6q08                 15     16P 0                                      Capi lanciati x
BET6 D  e6q09                 17     18P 0                                      Capi lanciati x
BET6 D  e6q10                 19     20P 0                                      Capi lanciati x
BET6 D  e6q11                 21     22P 0                                      Capi lanciati x
BET6 D  e6q12                 23     24P 0                                      Capi lanciati x
BET6 D  e6q13                 25     26P 0                                      Capi lanciati x
BET6 D  e6q14                 27     28P 0                                      Capi lanciati x
BET6 D  e6q15                 29     30P 0                                      Capi lanciati x
BET6 D  e6q16                 31     32P 0                                      Capi lanciati x

BET2 D                 DS
BET6 D  wss                    1     32P 0                                      Capi scatola
BET6 D                                     DIM(16)                              File SCACM01K
BET6 D  wss01                  1      2P 0                                      Capi scatola
BET6 D  wss02                  3      4P 0                                      Capi scatola
BET6 D  wss03                  5      6P 0                                      Capi scatola
BET6 D  wss04                  7      8P 0                                      Capi scatola
BET6 D  wss05                  9     10P 0                                      Capi scatola
BET6 D  wss06                 11     12P 0                                      Capi scatola
BET6 D  wss07                 13     14P 0                                      Capi scatola
BET6 D  wss08                 15     16P 0                                      Capi scatola
BET6 D  wss09                 17     18P 0                                      Capi scatola
BET6 D  wss10                 19     20P 0                                      Capi scatola
BET6 D  wss11                 21     22P 0                                      Capi scatola
BET6 D  wss12                 23     24P 0                                      Capi scatola
BET6 D  wss13                 25     26P 0                                      Capi scatola
BET6 D  wss14                 27     28P 0                                      Capi scatola
BET6 D  wss15                 29     30P 0                                      Capi scatola
BET6 D  wss16                 31     32P 0                                      Capi scatola

BET2 D                 DS
BET6 D  rigaord                1      5  0
BET6 D  $numcpa                1      2  0
BET6 D  $numer                 3      5  0

BET2 D                 DS
BET6 D  $numerobox             1      8
BET6 D  $seriebox              1      3
BET6 D  $numbox                4      8
PG1   *-------------------------------------------------------
 "   D $pt_qta_legcm   s               *   INZ(%ADDR(U1Q01))
 "   D U1Q             s                   DIM(16) like(U1Q01)
 "   D                                     based($pt_qta_legcm)
 "    *-------------------------------------------------------
 "   D $pt_qta_legor   s               *   INZ(%ADDR(HMQ01))
 "   D HMQ             s                   DIM(16) like(HMQ01)
 "   D                                     based($pt_qta_legor)
 "    *-------------------------------------------------------
 "   D $pt_qta_orcri   s               *   INZ(%ADDR(AUA01))
 "   D AUAQ            s                   DIM(16) like(AUA01)
 "   D                                     based($pt_qta_orcri)
 "   D $pt_qta_orcriv  s               *   INZ(%ADDR(AUR01))
 "   D AURQ            s                   DIM(16) like(AUR01)
 "   D                                     based($pt_qta_orcriv)
 "    *-------------------------------------------------------
 "   D x               s              2  0 inz
PG1   *-------------------------------------------------------

     *
     D                SDS                                                       Datos
     D  @PGM                   1     10                                         Progra
     D  @JOB                 244    253                                         Id Job
     D  @USER                254    263                                         Id Usu
     D  @SDS81                81    269                                         Contro
     D  @NJOB                264    269  0                                      Contro
     D  @PARMS           *PARMS
     D  @NSEN                227    232  0
     D  @STAT            *STATUS
     D  @SDS                   1    253
     D  @PSSR1                 1    253                                         Area E
     D  @PSSR2               254    429                                         Area E
     *
     *****************************************************************
     *    Main Program                                               *
     *****************************************************************
     *
     C                   exsr      defcam
     *
     * Inizio lettura
     C     *loval        setll     SNS0102L
     C                   do        *hival
     C                   read      SNS0102L
     C                   if        %EOF
     C                   leave
     C                   endif
     *
PG1  c                   eval      $assegna='0'
 "    *
 "   c                   if        s1cblk='SS'
 "   c                   eval      $assegna ='1'
 "   c                   Exsr      Rec_ord_box
 "    * ----------------------------------------------
 "    * Errore box non trovato in sns
 "    * Metto in 'S' e scrivo log errori x segnalare
 "   c                   if        T_err_box='1'
 "   c                   eval      $errore = 'E'
 "   c                   clear                   zbferr
 "   c                   eval      msgeab = 'SNS4876'
 "   c                   Exsr      wrt_errore
 "   c                   eval      S1flan = 'S'
 "   c                   update    SNS01
 "   c                   iter
 "   c                   Endif
 "    *
 "   c                   Endif
PG1   * --------------------------------
     *
     c                   IF        S1STNW='RIC' OR
     c                             S1STNW='CLD'
     c                   eval      $doppione=*off
     C     ksns0101l     setll     SNS0101L
     C                   do        *hival
     C     ksns0101l     reade     SNS0101L
     C                   if        %EOF
     C                   leave
     C                   endif
     * REK sns0100f ricevuto doppio:
     c                   if        $rlS1FLAN<>*BLANKS
     c                   eval      $doppione=*on
     c                   Leave
     C                   endif
     c                   enddo
     *
     c                   if        $doppione=*on
     C                   eval      S1flan = 'D'
PG1  c                   exsr      Rip_ord_box
     C                   update    SNS01
     C                   iter
     C                   endif
     C                   ENDIF
     *
     *--Compilo la data con quella del giorno oppure se forzata quella di ricezione magazzino
     C                   Exsr      SET_DATA
     *
     * Pulizia flag errore e file WPFER
     C                   clear                   $errore
     C                   clear                   $iter
     C                   clear                   $ubor
     C                   clear                   $tdor
     C                   clear                   $pdor
     C                   clear                   $ndor
     C                   clear                   $nror
     C                   clear                   $rigalis
     C                   Exsr      Pulisci_err
     *
     * Se non vengono valorizzati gli statu } un errore
     C                   if        S1stnw = *blanks
     C                             and S1stol = *blanks
     C                   eval      $errore = 'E'
     C                   clear                   zbferr
     C                   eval      uborab = S1ubor
     C                   eval      tdorab = S1tdor
     C                   eval      pdorab = S1pdor
     C                   eval      ndorab = S1ndor
     C                   eval      ubptab = S1ubpt
     C                   eval      protab = S1prot
     C                   eval      nrorab = S1nror
     C                   eval      msgeab = 'SNS8324'
     C                   exsr      Errore
     C                   eval      S1flan = 'E'
PG1  c                   exsr      Rip_ord_box
     C                   update    SNS01
     C                   iter
     C                   endif
     *
     * Trascodifico la CPA (la regola nuova e' il
     * campo S1NROR i primi due byte)
     C                   eval      rigaord = S1nror
     C                   clear                   $ccpa
     C                   if        $numcpa <> 0
     C     $numcpa       chain     ctppn01l
     C                   if        not %found
     C                   eval      $errore = 'E'
     C                   clear                   zbferr
     C                   eval      uborab = S1ubor
     C                   eval      tdorab = S1tdor
     C                   eval      pdorab = S1pdor
     C                   eval      ndorab = S1ndor
     C                   eval      ubptab = S1ubpt
     C                   eval      protab = S1prot
     C                   eval      nrorab = S1nror
     C                   eval      msgeab = 'SNS1417'
     C                   exsr      Errore
     C                   eval      S1flan = 'E'
PG1  c                   exsr      Rip_ord_box
     C                   update    SNS01
     C                   iter
     C                   else
     C                   eval      $ccpa = pntpps
     C                   clear                   $numcpa
     C                   endif
     C                   else
     C                   clear                   $ccpa
     C                   endif
     *
     *
     * Verifico che la CPM trascodificata esista in tabella SNS
     C                   if        $ccpa <> *blanks
     C                   Movel(P)  'CPM'         $ctab
     C                   Movel(P)  $ccpa         $cele
     C                   Exsr      Ctl_decotab
     C                   If        @DLI3 = '1'
     C                   eval      $errore = 'E'
     C                   clear                   zbferr
     C                   eval      uborab = S1ubor
     C                   eval      tdorab = S1tdor
     C                   eval      pdorab = S1pdor
     C                   eval      ndorab = S1ndor
     C                   eval      ubptab = S1ubpt
     C                   eval      protab = S1prot
     C                   eval      nrorab = S1nror
     C                   eval      ccpaab = $ccpa
     C                   eval      msgeab = 'SNS3844'
     C                   exsr      Errore
     C                   eval      S1flan = 'E'
PG1  c                   exsr      Rip_ord_box
     C                   update    SNS01
     C                   iter
     C                   endif
     C                   endif
     *
     * Se } un annullamentio e non esiste ORCRI, pongo lo status come sospeso.
     C                   Exsr      Ann_no_ORCR
     C                   if        $iter <> *blanks
     C                   iter
     C                   endif
     *
     *
     * Verifico legame tra OVDRG e ORCRI
     C                   if        S1tdor <> *blanks
     C     Kriga         chain     orcri28k
     C                   if        not %found
     C     Krigap        chain     orcri28k
     C                   if        not %found
     C                   eval      $ubor = S1ubor
     C                   eval      $tdor = S1tdor
     C                   eval      $pdor = S1pdor
     C                   eval      $ndor = S1ndor
     C                   eval      $nror = S1nror
     C                   exsr      Test_OADRG
     C                   if        $iter = *blanks
     C                   eval      $errore = 'E'
     C                   clear                   zbferr
     C                   eval      uborab = S1ubor
     C                   eval      tdorab = S1tdor
     C                   eval      pdorab = S1pdor
     C                   eval      ndorab = S1ndor
     C                   eval      ubptab = S1ubpt
     C                   eval      protab = S1prot
     C                   eval      nrorab = S1nror
     C                   eval      msgeab = 'SNS8145'
     C                   exsr      Errore
     C                   eval      S1flan = 'E'
PG1  c                   exsr      Rip_ord_box
     C                   update    SNS01
     C                   iter
     C                   endif
     c                   endif
     *
     C                   endif
     *
     c                   if        AUSTAT='SD'
     C                   eval      S1flan = 'S'
PG1  c                   exsr      Rip_ord_box
     C                   update    SNS01
     C                   iter
     C                   endif
     C                   if        AUTSTO='C'   and
     c                             AUSTAT='RM'  and
     c                             S1STNW='RIC'
     C                   eval      S1flan = 'S'
PG1  c                   exsr      Rip_ord_box
     C                   update    SNS01
     C                   iter
     C                   endif
     *
cer ac*                  if        austat='BO'
cer ac*                  if        S1stnw='ANN' and s1stol='ASO'
     c*                  if        S1stnw='ANN' and s1stol='ASO' or
     c*                            S1stnw=*blank and s1stol='***'
cer aC*                  eval      S1flan = *blank
    ac*                  else
    aC*                  eval      S1flan = 'S'
    aC*                  update    SNS01
    aC*                  iter
    ac*                  endif
cer ac*                  endif
     c
     * Assegnazione BO da assegnazioartic/modello/colore
     C*                  if        S1stnw=*blank and S1stol='***'
     C*                  exsr      Asseg_BO_vasca
     c*                  endif
      *
     c                   if        austat='SB'
     C                   eval      S1flan = 'S'
PG1  c                   exsr      Rip_ord_box
     C                   update    SNS01
     C                   iter
     C                   endif
     *
     c
     C     aumags        chain     magan02k
     C                   if        a003bz <> ##cudb
     c                             and S1UBUS<>'COM'
cer ac*                            (s1stol<>'ASO' or S1UBUS<>'ANN')              berizzi 21102013
cer  C* l'errore deve essere dato solo se non stiamo elaborando
cer  C* assegnazioni o disassegnazioni da vasca
cer  C                   select
 "   C                   When       s1stol = 'ASO' and S1STNW = 'ANN'
 "   C                   When       s1stol = 'VAS' and S1STNW = 'ANN'
 "   C                   When       S1STNW = 'ASO'
cer  C                   other
     C                   clear                   zbferr
     C                   eval      uborab = S1ubor
     C                   eval      tdorab = S1tdor
     C                   eval      pdorab = S1pdor
     C                   eval      ndorab = S1ndor
     C                   eval      ubptab = S1ubpt
     C                   eval      protab = S1prot
     C                   eval      nrorab = S1nror
     C                   eval      cstgab = aucstg
     C                   eval      cudbab = aucudb
     C                   eval      ccliab = auccli
     C                   eval      nordab = aunord
     C                   eval      nrigab = aunrig
     C                   eval      srigab = ausrig
     C                   eval      ccpaab = $ccpa
     C                   eval      magsab = aumags
     C                   eval      msgeab = 'SNS3486'
     C                   exsr      Errore
     C                   eval      S1flan = 'E'
PG1  c                   exsr      Rip_ord_box
     C                   update    SNS01
     C                   iter
cer  C                   endsl
     C                   endif
     *
     C                   if        $iter = 'Y'
     C                   eval      S1flan = 'S'
PG1  c                   exsr      Rip_ord_box
     C                   update    SNS01
     C                   iter
     C                   endif
     * Se la cpm di ORCRI e' uguale a quella trascodificata
     * allora la pulisco
     C                   if        $ccpa = auccpm
     C                   clear                   $ccpa
     C                   endif
     *
     * Verifico esistenza ORPRA
     C                   IF        AUSTAT <> 'BO'
     C                   clear                   $masm
     C                   eval      e6nopr = aunord
     C                   eval      e6ropr = aunrig
     C                   eval      e6srpr = ausrig
     C                   if        $ccpa <> *blanks
     C     Korpra        chain(N)  orpra55k
     C                   else
     C     KorpraSNS     chain(N)  orpra55k
     C                   endif
     C                   if        not %found
     C                   eval      $errore = 'E'
     C                   clear                   zbferr
     C                   eval      uborab = S1ubor
     C                   eval      tdorab = S1tdor
     C                   eval      pdorab = S1pdor
     C                   eval      ndorab = S1ndor
     C                   eval      ubptab = S1ubpt
     C                   eval      protab = S1prot
     C                   eval      nrorab = S1nror
     C                   eval      cstgab = aucstg
     C                   eval      cudbab = aucudb
     C                   eval      ccliab = auccli
     C                   eval      nordab = aunord
     C                   eval      nrigab = aunrig
     C                   eval      srigab = ausrig
     C                   eval      noprab = aunord
     C                   eval      roprab = aunrig
     C                   eval      srprab = ausrig
     C                   eval      ccpaab = $ccpa
     C                   eval      msgeab = 'SNS8146'
     C                   exsr      Errore
     C                   eval      S1flan = 'E'
PG1  c                   exsr      Rip_ord_box
     C                   update    SNS01
     C                   iter
     C                   else
     C                   eval      $masm = e6masm
     C                   endif
     C                   endif

     C                   endif
      *
     c                   eval      $agnon=*blank
     c                   if        s1ubus='COM'
     C                   clear                   zp@gen
     C                   eval      cprmds = 'AGN'
     C                   eval      cudbds = ##cudb
     C                   eval      magsds = e6masm
      *
     C                   call      'P@GENCL'
     C                   parm                    zp@gen
      *
     C                   if        critds =*blanks
     c                   eval      $agnon=fds01
     c                   endif
     c                   endif
     c
BET  * -----------------------------------------------------------------
 "   * Trascodifico la schiera taglie CSIS passata da SNS0100F verso SNS
 "   C                   eval      RLNS   = *Zeros
 "   C                   eval      $trascotgl = '2'
 "   C                   eval      $ttag = auttag
 "   C                   eval      tgl01  = s1qn01
 "   C                   eval      tgl02  = s1qn02
 "   C                   eval      tgl03  = s1qn03
 "   C                   eval      tgl04  = s1qn04
 "   C                   eval      tgl05  = s1qn05
 "   C                   eval      tgl06  = s1qn06
 "   C                   eval      tgl07  = s1qn07
 "   C                   eval      tgl08  = s1qn08
 "   C                   eval      tgl09  = s1qn09
 "   C                   eval      tgl10  = s1qn10
 "   C                   eval      tgl11  = s1qn11
 "   C                   eval      tgl12  = s1qn12
 "   C                   eval      tgl13  = s1qn13
 "   C                   eval      tgl14  = s1qn14
 "   C                   eval      tgl15  = s1qn15
 "   C                   eval      tgl16  = s1qn16
 "   C                   Exsr      Trasco_tgl
BET  C                   eval      RLNS   = RLN
BET  C                   eval      RLNC   = RLN
PG1   *-----------------------------------------
PG1  c                   eval      RLNX   = RLN
PG1   * ------------------------------------------
PG1   * Se passato box sommo qta totale da schiera
PG1  c                   if        $assegna ='1'
PG1  c                   eval      s1toqn = %xfoot(RLNX)
PG1  c                   endif
PG1   *-------------------------------------------------------------
PG1   * Se $assegna attivo sto facendo rientri di cravatta da Nr.Box
     c                   exsr      tipo_avanza
     *
     C                   if        $iter   = 'Y'
     C                   eval      S1flan = 'S'
PG1  c                   exsr      Rip_ord_box
     C                   update    SNS01
     C                   rolbk
     c                   if        $sovraprod
     * X eventuali impegni da annullare  (x sovraproduzione):
     c                   call      'MG126CL'
     C                   PARM                    KPJBA
     c                   endif
     C                   iter
     C                   endif
     *
     C                   if        $controllo = *OFF
     C                   eval      $errore = 'E'
     C                   clear                   zbferr
     C                   eval      msgeab = 'SNS8270'
     C                   exsr      Errore
     C                   endif
     *
      * la seconda scelta maglieria secondo riunione tra Ale/Lello /Berizzi
      * del 3/7/2013 verra' gestita dal BIGFISH. tramite flag S1CBLK='2S'
      * fara' le stesse decurtazioni del mg041
      *
     C                   if        $errore=*blank and s1cblk='2S'
     C                   exsr      Sr_amain
     C                   Endif
      *
     C                   if        $errore = *blanks
     C                   exsr      Salva_SNS01
     C                   eval      S1flan = 'A'
PG1  c                   exsr      Rip_ord_box
     C                   update    SNS01
     *-----------------------------------------------------------*
     *- Riaggiorno aVPRD per tornare a qt.a reali
     c     KAVprd3       chain     avprd03K
     c                   if        %found(avprd03k)
      *
     c                   eval      $ICQTOR = $ICQTOR / 2
     c                   eval      $ICT02 =  $ICT02  / 2
     c                   eval      $ICT03 =  $ICT03  / 2
     c                   eval      $ICT04 =  $ICT04  / 2
     c                   eval      $ICC02 =  $ICC02   / 2
     c                   eval      $ICC03 =  $ICC03  / 2
     c                   eval      $ICC04 =  $ICC04  / 2
     c                   eval      $ICQCRE = $ICQCRE / 2
      *
     c                   update    $avprd
      *
     c                   Endif
      *
     c                   commit
     *-----------------------------------------------------------*

     *-----------SOVRAPRODUZIONE---------------------------------*
     c                   if        $sovraprod
     * X eventuali impegni da aggiornare (x sovraproduzione):
     c                   call      'MG124'
     C                   PARM                    KPJBA
     c                   endif
     C                   else
     C                   eval      $chiamaerr = *ON
     C                   rolbk
     c                   if        $sovraprod
     * X eventuali impegni da annullare  (x sovraproduzione):
     c                   call      'MG126CL'
     C                   PARM                    KPJBA
     c                   endif
     C                   eval      S1flan = 'E'
PG1  c                   exsr      Rip_ord_box
     C                   update    SNS01
     C                   endif
     *-----------SOVRAPRODUZIONE---------------------------------*
     *
     C                   enddo
     * Fine
     C                   exsr      fine
     *
PG1  *****************************************************************
 "   * Recupero da box ordine sns
 "   *****************************************************************
 "   c     Rec_ord_box   Begsr
 "    *----------------------------------------
 "    * Memorizzo il numero box preventivamente in caso
 "    * di errore ripristino i dati
 "   c                   eval      xS1UBOR = S1UBOR
 "   c                   eval      xS1TDOR = S1TDOR
 "   c                   eval      xS1PDOR = S1PDOR
 "   c                   eval      xS1NDOR = S1NDOR
 "   c                   eval      xS1NROR = S1NROR
 "    *----------------------------------------
 "    * Recupero da box l'ordine Bo che sto elaborando e
 "    * e trascodifico a sns
 "   c     KBoarg        chain     BOARG                              99
 "   c   99              eval      T_err_box='1'
 "   c   99              Leavesr
 "    *----------------------------------------
 "    * Da ordine csis recupero ordine sns
 "   c     KOrcriBoa     chain(n)  ORCRI28K                           99
 "   c   99              eval      T_err_box='1'
 "   c   99              Leavesr
 "    * ----------------------------------
 "    * Se trovato memorizzo ordine BO SNS
 "   c                   eval      $$CUDB = AUCUDB
 "   c                   eval      $$CSTG = AUCSTG
 "   c                   eval      $$CCLI = AUCCLI
 "   c                   eval      $$NORD = AUNORD
 "   c                   eval      $$NRIG = AUNRIG
 "   c                   eval      $$SRIG = AUSRIG
 "    *----------------------------------------
 "    * Recupero da box l'ordine produttivo
 "    * Compongo la key
 "   c                   move      *blank        T_err_box         1
 "   c                   movel(p)  S1NDOR        DS_box
 "    * --------------------------------
 "    * Verifico se serie di 2 o 3 byte
 "   c                   if        Box_fil ='0'
 "   c                   eval      xO5SBOX=Box_ser2
 "   c                   else
 "   c                   eval      xO5SBOX=Box_ser3
 "   c                   endif
 "    *
 "   c                   move      Nr_box        xO5NUMB
 "   c                   eval      xO5ANNB=%dec(%trim(S1PDOR):4:0)
 "   c                   z-add     S1NROR        xO5NRGB
 "    *-----------------------------------------------------
 "    * Verifico se esiste rif.box su file sns imbor.
 "   c     KImbor        chain     RImbor                             99
 "   c   99              eval      T_err_box='1'
 "   c   99              Leavesr
 "    * -------------------------------------------------
 "    * Recupero ordine csis da ordine produttivo del box
 "   c     KorcriBox     chain(n)  ORCRI01                            99
 "   c   99              eval      T_err_box='1'
 "   c   99              Leavesr
 "    * -----------------------------------------------
 "    * Rimpiazzo i campi ordine con quelli recuperati
 "   c                   move      AU$UBO        S1UBOR
 "   c                   move      AU$TDO        S1TDOR
 "   c                   move      AU$PDO        S1PDOR
 "   c                   move      AU$NDO        S1NDOR
 "   c                   move      AU$NRO        S1NROR
 "    *
 "   c                   Endsr
 "   *****************************************************************
 "   * Ripristino in numero box in caso di test $assegna ='1'
 "   *****************************************************************
 "   c     Rip_ord_box   Begsr
 "    *------------------------------------------------
 "    * Riprsitino i dati box in caso di errore
 "   c                   if        $assegna ='1'
 "   c                   eval      S1UBOR = xS1UBOR
 "   c                   eval      S1UBOR = xS1UBOR
 "   c                   eval      S1TDOR = xS1TDOR
 "   c                   eval      S1PDOR = xS1PDOR
 "   c                   eval      S1NDOR = xS1NDOR
 "   c                   eval      S1NROR = xS1NROR
 "   c                   Endif
 "    *
PG1  c                   Endsr
     *****************************************************************
     *    tipologia avanzamento
     *****************************************************************
     c     Tipo_avanza   Begsr
      *
     C                   eval      $controllo = *ON
     *
VEN   * se non e' famiglia 3 maglia ma ha un collaudo virtuale si comporta come la maglieria
VEN   * al momento e' utilizzato per la fam.5 jersey di Agnona
VEN  C                   exsr      ctr_colv                                     ctrl se collaudo vir
      *
     C                   exsr      s_ccpd
     C                   eval      ifnewa=*blank
     *
     * se l'avanzamento e'gestito con nuova procedura e' valorizzato gruppo di fase su avprd
BET9 C     korcpa        Chain(n)  avprd03k
     c                   if        %found(avprd03k) and $icgrpf<>0
     C                   eval      ifnewa='Y'
     c                   endif
BET9 C     aucfam        Chain     Faman01k
     *

BETA * Richiamo modulo
 "   C                   If        S1stnw = 'CLD'   AND
 "   C                             S1stol = *blanks OR
 "   C                             S1stnw = 'RIC'   AND
 "   C                             S1stol = *blanks
 "   C                   ExSr      CallLN782
BETA C                   EndIf
     * In base agli status compilati nell'archivio, eseguo diverse operazioni
     C                   select
     *
     * Caricamento Magazzino WMS x maglieria
BET9 C                   When      %FOUND(FAMAN01K) AND
BET9 C                             fnobia = 'Y'     AND
BET9 C                             ifnewa = 'Y'     AND
BET9 C                             S1stnw = 'CLD'   AND
     C                             S1stol = *blanks
     C
     C                   ExSr      Maglieria
     C                   Eval      $controllo = *ON
      *
     *
     * Caricamento Magazzino WMS commercializzato x maglieria
BET9 C                   When      %FOUND(FAMAN01K) AND
BET9 C*                            ABFMAG = 'Y'     AND
BET9 C                             fnobia = 'Y'     AND
     C                             S1stnw = 'RIC'   AND
     C                             S1stol = *blanks
     c
     C                   ExSr      Maglieria
     C                   Eval      $controllo = *ON
      *
BET9 C                   When      %FOUND(FAMAN01K) AND
BET9 C                             ABFMAG = 'N'     AND
BET9 C                             colvir = 'Y'     AND
     C                             S1stnw = 'RIC'   AND
     C                             S1stol = *blanks
     C                   ExSr      Maglieria
     C                   Eval      $controllo = *ON
     *
     * Caricamento da Magazzino WMS, scarico Magazzino transito SNS
     C     S1stnw        wheneq    'MAG'
     C     S1stol        andeq     *blanks
     C                   exsr      Magazzino
     C                   eval      $controllo = *ON
     *
     * Rientro a magazzino righe Con Conto Lavoro - NO Bulk
     C     S1stnw        wheneq    'CLD'
     C     S1stol        andeq     *blanks
     C     aucblk        andeq     *blanks
     C                   eval      $controllo = *ON
     C                   exsr      Agg_NO_BLK
     C                   IF        $errore = *blanks
MMD  C                   exsr      FattuPassiv
     C                   endif
     *
     * Rientro a magazzino righe Con Conto Lavoro - Bulk
     C     S1stnw        wheneq    'CLD'
     C     S1stol        andeq     *blanks
     C     aucblk        andne     *blanks
     C                   eval      $controllo = *ON
     C                   exsr      Agg_BLK
     C                   IF        $errore = *blanks
MMD  C                   exsr      FattuPassiv
     C                   endif
     *
     * Rientro a magazzino righe commercializzate - NO BULK
     C     S1stnw        wheneq    'RIC'
     C     S1stol        andeq     *blanks
     C     aucblk        andeq     *blanks
     C                   eval      $controllo = *ON
     C                   exsr      Com_no_BLK
     *
     * Rientro a magazzino righe commercializzate - BULK
     C     S1stnw        wheneq    'RIC'
     C     S1stol        andeq     *blanks
     C     aucblk        andne     *blanks
     C                   eval      $controllo = *ON
     C                   exsr      Com_BLK
     *
     * Selezionati in Proposta
     C     S1stnw        wheneq    'LIS'
     C                   exsr      Proposta
     *
     * Spedizione (assume due valori: 'ISP' se genero un DDT a fronte di una
     * precedente lista di prelievo, altrimenti considero avanzamento 'SPE'
     * come borderaux e DDT)
     C     S1stnw        wheneq    'ISP'
     C     S1stnw        oreq      'SPE'
     C                   exsr      Sped_ctl
     C                   IF        $errore = *blanks and aucblk = '   '
     C                   exsr      Sped_agg
     C                   endif
     *
     * Fatturazione
     C     S1stnw        wheneq    'FAT'
     C                   exsr      Fatt_CTL
     C                   IF        $errore = *blanks and aucblk = '   '
     C                   exsr      Fatt_AGG
     C                   endif
     *
     * Genero box
     C     S1stnw        wheneq    'BOX'
     C     S1stol        andeq     *blanks
      * 10-10-2013 dopo aver parlato con Berizzi tolgo questa creazione dei box
      * creata in passato per procedura  navettamenti che non esistono piu
     C                   exsr      Crea_box
     C                   eval      $controllo = *ON
     *
     * Capi prodotti in piu'
     C     S1stnw        wheneq    '051'
     C     S1stnw        oreq      '059'
     C     S1stnw        oreq      '068'
     C                   exsr      capiprodp
     * Capi prodotti in meno
     C     S1stnw        wheneq    '151'
     C     S1stnw        oreq      '159'
     C     S1stnw        oreq      '067'
     C     S1stol        andeq     *blank
     C                   exsr      capiprodm
     * Cambio colore in aggiunta a riga ordine da trovare
     C     S1stnw        wheneq    '067'
     C     S1stol        andeq     '***'
     C                   exsr      cambiocolo
     * Assegnamento da vasca di ordini BO
     C*    S1stnw        wheneq    'ASO'
     C*                  exsr      asse_vasca
     * Preannullamento di capi a magazzino WMS/CSIS
     C     S1stnw        wheneq    'ANN'
     C     S1stol        andeq     *blanks
     C     Austat        andne     'BO'
     C                   exsr      Preannulla
     *
     * Annullamento proposte/spedizioni/fatturazione
     C     S1stnw        wheneq    'ANN'
     C     S1stol        andne     *blanks
     C     S1stol        andne     'ASO'
     C     Austat        andne     'BO'
     C                   exsr      Annullamento
PG1  * ----------------------------------------------------
 "   * Assegnazione da vasca
 "   c     S1stnw        wheneq    'ASO'
 "   c     S1stol        andeq     *blanks
 "   c     aucblk        andne     *blanks
 "   c                   exsr      Ass_vasca
 "   * ----------------------------------------------------
 "   * Annullamento da vasca
 "   c     S1stnw        wheneq    'ANN'
 "   c     S1stol        andeq     'VAS'
 "   c     aucblk        andne     *blanks
 "   c                   exsr      Ann_vasca
 "   * ----------------------------------------------------
 "   * Versamenti in vasca ordini produttivi...
 "   c     S1stnw        wheneq    'VAS'
 "   c     S1stol        andeq     'VEN'
 "   c                   exsr      Ver_vasca
PG1  * ----------------------------------------------------
     *
     C                   other
     C                   eval      $errore = 'E'
     C                   clear                   zbferr
     C                   eval      uborab = S1ubor
     C                   eval      tdorab = S1tdor
     C                   eval      pdorab = S1pdor
     C                   eval      ndorab = S1ndor
     C                   eval      ubptab = S1ubpt
     C                   eval      protab = S1prot
     C                   eval      nrorab = S1nror
     C                   eval      msgeab = 'SNS8270'
     C                   exsr      Errore
     *
     C                   endsl
      *
     c                   Endsr
PG1  *****************************************************************
 "   *  Assegnazione da vasca
 "   *****************************************************************
 "   c     Ass_vasca     begsr
 "    * -------------------------------------------------------
 "    * Verifico se qta.da assegnare da vasca > di qta.in essere
 "   c                   if        s1toqn>aucqta
 "   c                   eval      $errore = 'E'
 "   c                   clear                   zbferr
 "   c                   eval      msgeab = 'SN10325'
 "   c                   Exsr      wrt_errore
 "   c                   leavesr
 "   c                   endif
 "    * -------------------------------------------------------------
 "    * Verifico se singole qt.a per taglia in assegnamento non
 "    * superano le qt.a in essere.
 "   c                   for       x=1 to 16
 "   * --------------------------
 "   * se ho qt.a da assegnare
 "   c                   if        RLNX(x) > 0
 "   * se qta. da ass.maggiori a quelle in essere.
 "   c                   if        (AURQ(x) + RLNX(x)) > AUAQ(x)
 "   c                   eval      $errore = 'E'
 "   c                   clear                   zbferr
 "   c                   eval      msgeab = 'SN10325'
 "   c                   Exsr      wrt_errore
 "   c                   leavesr
 "   c                   Endif
 "   *
 "   c                   endif
 "   *
 "   c                   endfor
 "    * -----------------------
 "    * -----------------------
 "    * Assegno qt.a versate
 "   c                   eval      aucqrv=aucqrv+S1TOQN
 "   c                   eval      aur01=aur01+RLNX(1)
 "   c                   eval      aur02=aur02+RLNX(2)
 "   c                   eval      aur03=aur03+RLNX(3)
 "   c                   eval      aur04=aur04+RLNX(4)
 "   c                   eval      aur05=aur05+RLNX(5)
 "   c                   eval      aur06=aur06+RLNX(6)
 "   c                   eval      aur07=aur07+RLNX(7)
 "   c                   eval      aur08=aur08+RLNX(8)
 "   c                   eval      aur09=aur09+RLNX(9)
 "   c                   eval      aur10=aur10+RLNX(10)
 "   c                   eval      aur11=aur11+RLNX(11)
 "   c                   eval      aur12=aur12+RLNX(12)
 "   c                   eval      aur13=aur13+RLNX(13)
 "   c                   eval      aur14=aur14+RLNX(14)
 "   c                   eval      aur15=aur15+RLNX(15)
 "   c                   eval      aur16=aur16+RLNX(16)
 "    *
 "   c                   eval       AUFPVA ='Y'
 "    * -------------------------------------------------
 "    * Aggiornamento ORCRI
 "   c                   update    rorcri
 "   c                   clear                   ZVE500
 "    * -------------------------------------------------
 "    * Scrittura variazioni
 "   c                   eval      ANMCGF = 'Y'
 "   c                   Eval      Cvargf = $CauAss
 "   c                   Eval      BLKCGF =S1MAGA
 "   c                   Eval      varigf = S1TOQN
 "   c                   Eval      gfv01  = RLNX(1)
 "   c                   Eval      gfv02  = RLNX(2)
 "   c                   Eval      gfv03  = RLNX(3)
 "   c                   Eval      gfv04  = RLNX(4)
 "   c                   Eval      gfv05  = RLNX(5)
 "   c                   Eval      gfv06  = RLNX(6)
 "   c                   Eval      gfv07  = RLNX(7)
 "   c                   Eval      gfv08  = RLNX(8)
 "   c                   Eval      gfv09  = RLNX(9)
 "   c                   Eval      gfv10  = RLNX(10)
 "   c                   Eval      gfv11  = RLNX(11)
 "   c                   Eval      gfv12  = RLNX(12)
 "   c                   Eval      gfv13  = RLNX(13)
 "   c                   Eval      gfv14  = RLNX(14)
 "   c                   Eval      gfv15  = RLNX(15)
 "   c                   Eval      gfv16  = RLNX(16)
 "   c                   ExSr      Savorcri
 "   c                   exsr      Svariaz
 "    *---------------------------------------
 "    * Aggiorno legor (richiamo pgm OR184)
 "   c                   clear                   ZOR184
 "   c                   eval      flupB7 = 'Y'
 "   c                   eval      flomb7 = 'Y'
 "   c                   eval      cudbB7 = aucudb
 "   c                   eval      ccliB7 = auccli
 "   c                   eval      cstgB7 = aucstg
 "   c                   eval      nordB7 = aunord
 "   c                   eval      nrigB7 = aunrig
 "   c                   eval      srigB7 = ausrig
 "    *
 "   c                   Eval      cqtab7 = s1toqn
 "   c                   Eval      QT01B7 = RLNX(1)
 "   c                   Eval      QT02B7 = RLNX(2)
 "   c                   Eval      QT03B7 = RLNX(3)
 "   c                   Eval      QT04B7 = RLNX(4)
 "   c                   Eval      QT05B7 = RLNX(5)
 "   c                   Eval      QT06B7 = RLNX(6)
 "   c                   Eval      QT07B7 = RLNX(7)
 "   c                   Eval      QT08B7 = RLNX(8)
 "   c                   Eval      QT09B7 = RLNX(9)
 "   c                   Eval      QT10B7 = RLNX(10)
 "   c                   Eval      QT11B7 = RLNX(11)
 "   c                   Eval      QT12B7 = RLNX(12)
 "   c                   Eval      QT13B7 = RLNX(13)
 "   c                   Eval      QT14B7 = RLNX(14)
 "   c                   Eval      QT15B7 = RLNX(15)
 "   c                   Eval      QT16B7 = RLNX(16)
 "   c                   movel     zor184        kpjbu
 "    *
 "   c                   call      'OR184'
 "   c                   parm                    kpjba
 "    *
 "   c                   Endsr
 "   *****************************************************************
 "   *  Annullamento da vasca
 "   *****************************************************************
 "   c     Ann_vasca     begsr
 "    * -------------------------------------------------------
 "    * Verifico se qta.annullate da vasca > di qta.in essere
 "   c                   if        s1toqn>aucqta or s1toqn>aucqrv
 "   c                   eval      $errore = 'E'
 "   c                   clear                   zbferr
 "   c                   eval      msgeab = 'SN10313'
 "   c                   Exsr      wrt_errore
 "   c                   leavesr
 "   c                   endif
 "    * -------------------------------------------------------------
 "    * Verifico se singole qt.a per taglia da annullare siano
 "    * minori o uguale a qt.a versate.
 "   c                   for       x=1 to 16
 "   * --------------------------
 "   * se ho qt.a da riassegnare
 "   c                   if        RLNX(x) > 0
 "   * se qta. da annullare minore uguale a quella in essere
 "   c                   if        (AURQ(x) -RLNX(x)) < *zero
 "   c                   eval      $errore = 'E'
 "   c                   clear                   zbferr
 "   c                   eval      msgeab = 'SN10324'
 "   c                   Exsr      wrt_errore
 "   c                   leavesr
 "   c                   Endif
 "   *
 "   c                   endif
 "   *
 "   c                   endfor
 "    * -----------------------
 "    * Decurto qt.a versate
 "   c                   eval      aucqrv=aucqrv-S1TOQN
 "   c                   eval      aur01=aur01-RLNX(1)
 "   c                   eval      aur02=aur02-RLNX(2)
 "   c                   eval      aur03=aur03-RLNX(3)
 "   c                   eval      aur04=aur04-RLNX(4)
 "   c                   eval      aur05=aur05-RLNX(5)
 "   c                   eval      aur06=aur06-RLNX(6)
 "   c                   eval      aur07=aur07-RLNX(7)
 "   c                   eval      aur08=aur08-RLNX(8)
 "   c                   eval      aur09=aur09-RLNX(9)
 "   c                   eval      aur10=aur10-RLNX(10)
 "   c                   eval      aur11=aur11-RLNX(11)
 "   c                   eval      aur12=aur12-RLNX(12)
 "   c                   eval      aur13=aur13-RLNX(13)
 "   c                   eval      aur14=aur14-RLNX(14)
 "   c                   eval      aur15=aur15-RLNX(15)
 "   c                   eval      aur16=aur16-RLNX(16)
 "    * -------------------------------------------------
 "    * Se qt.a versate a zero imposto flag AUFPVA = 'N'
 "   c                   if        (aur01 + aur02 + aur03 + aur04 +
 "   c                              aur05 + aur06 + aur07 + aur08 +
 "   c                              aur09 + aur10 + aur11 + aur12 +
 "   c                              aur13 + aur14 + aur15 + aur16) = *zero
 "   c                   eval       AUFPVA ='N'
 "   c                   Endif
 "    * -------------------------------------------------
 "    * Aggiornamento ORCRI
 "   c                   update    rorcri
 "   c                   clear                   ZVE500
 "    * -------------------------------------------------
 "    * Scrittura variazioni
 "   c                   eval      ANMCGF = 'Y'
 "   c                   Eval      Cvargf = $CauAnn
 "   c                   Eval      BLKCGF =S1MAGA
 "   c                   Eval      varigf = S1TOQN * (-1)
 "   c                   Eval      gfv01  = (RLNX(1) * -1)
 "   c                   Eval      gfv02  = (RLNX(2) * -1)
 "   c                   Eval      gfv03  = (RLNX(3) * -1)
 "   c                   Eval      gfv04  = (RLNX(4) * -1)
 "   c                   Eval      gfv05  = (RLNX(5) * -1)
 "   c                   Eval      gfv06  = (RLNX(6) * -1)
 "   c                   Eval      gfv07  = (RLNX(7) * -1)
 "   c                   Eval      gfv08  = (RLNX(8) * -1)
 "   c                   Eval      gfv09  = (RLNX(9) * -1)
 "   c                   Eval      gfv10  = (RLNX(10)* -1)
 "   c                   Eval      gfv11  = (RLNX(11)* -1)
 "   c                   Eval      gfv12  = (RLNX(12)* -1)
 "   c                   Eval      gfv13  = (RLNX(13)* -1)
 "   c                   Eval      gfv14  = (RLNX(14)* -1)
 "   c                   Eval      gfv15  = (RLNX(15)* -1)
 "   c                   Eval      gfv16  = (RLNX(16)* -1)
 "   c                   ExSr      Savorcri
 "   c                   exsr      Svariaz
 "    *-------------------------------------
 "    * Aggiorno qt.a legame legor da legcm
 "   c                   z-add     S1TOQN        t_qta_ass         5 0
 "   c     Korcpa        Setll     LEGCM03K
 "    *
 "   c                   Do        *HIVAL
 "    *
 "   c     Korcpa        Reade     LEGCM03K
 "    *
 "   c                   If        %EOF
 "   c                   Leave
 "   c                   EndIf
 "    *
 "   c                   if        A001U1='A'
 "   c                   Iter
 "   c                   EndIf
 "    *-----------------------------------------
 "    *Verifico se ho qta da riassegnare
 "   c     KlegAss       chain     RLEGOR                             99
 "    * ------------------------------------------------------
 "    * se trovato e non annullato e legato con tipo 'L'
 "    * e ho qta. da riassegnare
 "   c                   if        not *in99 and A001HM <> 'A' and
 "   c                             HMLEGE ='L' and
 "   c                             %xfoot(U1Q) > %xfoot(HMQ)
 "    * -------------------------------
 "    * Riassegno schiera qt.a su legor
 "   c                   for       x=1 to 16
 "   * --------------------------
 "   * se ho qt.a da riassegnare
 "   c                   if        RLNX(x) > 0
 "   *
 "   c                   if        RLNX(x) + hmq(x) >= u1q(x)
 "   c                   eval      t_qta_ass -= (u1q(x) - hmq(x))
 "   c                   eval      RLNX(x) = (u1q(x) - hmq(x))
 "   c                   eval      hmq(x) = u1q(x)
 "   c                   else
 "   c                   eval      hmq(x) += RLNX(x)
 "   c                   eval      t_qta_ass -=RLNX(x)
 "   c                   endif
 "   * --------------
 "   * Aggiorno legor
 "   c                   eval      HMQLEG =%xfoot(HMQ)
 "   c                   update    RLEGOR
 "   *
 "   c                   endif
 "   *
 "   c                   endfor
 "   *
 "   c                   Endif
 "    * --------------------------------------
 "    * Ho riassegnato tutte le quantita esco
 "   c                   if        t_qta_ass <= *zero
 "   c                   Leave
 "   c                   Endif
 "    *
 "   c                   Enddo
 "    *
 "   c                   Endsr
 "   *****************************************************************
 "   *  Versamenti a vasca ordini produttivi
 "   *****************************************************************
 "   c     ver_vasca     begsr
 "    *------------------------
 "    * Controlli se tipo ordine bulk ?
 "   c                   if        AUTORD <> '?'
 "   c                   eval      $errore = 'E'
 "   c                   clear                   zbferr
 "   c                   eval      msgeab = 'SNS9024'
 "   c                   Exsr      wrt_errore
 "   c                   leavesr
 "   c                   Endif
 "    * -----------------------------------------------------
 "    * Verifico se qta.versate non siano > di qta. in essere
 "   c                   if        s1toqn>aucqta
 "   c                   eval      $errore = 'E'
 "   c                   clear                   zbferr
 "   c                   eval      msgeab = 'SN10312'
 "   c                   Exsr      wrt_errore
 "   c                   leavesr
 "   c                   endif
 "    * -------------------------------------------------------------
 "    * Verifico se singole qt.a per taglia sia <= a quelle in essere
 "   c                   for       x=1 to 16
 "   * --------------------------
 "   * se ho qt.a da riassegnare
 "   c                   if        RLNX(x) > 0
 "   * se qta. da assegnare maggiore di quella in essere errore
 "   c                   if        RLNX(x) + AURQ(x) > AUAQ(x)
 "   c                   eval      $errore = 'E'
 "   c                   clear                   zbferr
 "   c                   eval      msgeab = 'SN10314'
 "   c                   Exsr      wrt_errore
 "   c                   leavesr
 "   c                   Endif
 "   *
 "   c                   endif
 "   *
 "   c                   endfor
 "    * -----------------------
     c                   exsr      maglieria
 "    *
 "   c                   Endsr
 "    *----------------------------------------------------------------
 "    * Subroutine per scrittura errore
 "    *----------------------------------------------------------------
 "   c     wrt_errore    begsr
 "    *
 "   c                   eval      uborab = S1ubor
 "   c                   eval      tdorab = S1tdor
 "   c                   eval      pdorab = S1pdor
 "   c                   eval      ndorab = S1ndor
 "   c                   eval      ubptab = S1ubpt
 "   c                   eval      protab = S1prot
 "   c                   eval      nrorab = S1nror
 "   c                   exsr      Errore
 "    *
PG1  c                   Endsr
     *****************************************************************
     *    Capi prodotti in piu su ordine bulk
     *****************************************************************
     C     capiprodp     begsr
      *
     c                   clear                   zve500
     C                   Eval      Cvargf = S1cava
     C                   Eval      varigf = S1TOQN
     C                   Eval      gfv01  = RLNS(1)
     C                   Eval      gfv02  = RLNS(2)
     C                   Eval      gfv03  = RLNS(3)
     C                   Eval      gfv04  = RLNS(4)
     C                   Eval      gfv05  = RLNS(5)
     C                   Eval      gfv06  = RLNS(6)
     C                   Eval      gfv07  = RLNS(7)
     C                   Eval      gfv08  = RLNS(8)
     C                   Eval      gfv09  = RLNS(9)
     C                   Eval      gfv10  = RLNS(10)
     C                   Eval      gfv11  = RLNS(11)
     C                   Eval      gfv12  = RLNS(12)
     C                   Eval      gfv13  = RLNS(13)
     C                   Eval      gfv14  = RLNS(14)
     C                   Eval      gfv15  = RLNS(15)
     C                   Eval      gfv16  = RLNS(16)
     c                   ExSr      Savorcri
     c
      *add nelle qta in essere
     c                   eval      aucqta=aucqta+S1TOQN
     c                   eval      aua01=aua01+RLNS(1)
     c                   eval      aua02=aua02+RLNS(2)
     c                   eval      aua03=aua03+RLNS(3)
     c                   eval      aua04=aua04+RLNS(4)
     c                   eval      aua05=aua05+RLNS(5)
     c                   eval      aua06=aua06+RLNS(6)
     c                   eval      aua07=aua07+RLNS(7)
     c                   eval      aua08=aua08+RLNS(8)
     c                   eval      aua09=aua09+RLNS(9)
     c                   eval      aua10=aua10+RLNS(10)
     c                   eval      aua11=aua11+RLNS(11)
     c                   eval      aua12=aua12+RLNS(12)
     c                   eval      aua13=aua13+RLNS(13)
     c                   eval      aua14=aua14+RLNS(14)
     c                   eval      aua15=aua15+RLNS(15)
     c                   eval      aua16=aua16+RLNS(16)
      *
     c                   eval      audtst=$$dat8
      *
      * add nelle qta versate a magazzino
      *
     c                   eval      aucqtm=aucqtm+S1TOQN
     c                   eval      aum01=aum01+RLNS(1)
     c                   eval      aum02=aum02+RLNS(2)
     c                   eval      aum03=aum03+RLNS(3)
     c                   eval      aum04=aum04+RLNS(4)
     c                   eval      aum05=aum05+RLNS(5)
     c                   eval      aum06=aum06+RLNS(6)
     c                   eval      aum07=aum07+RLNS(7)
     c                   eval      aum08=aum08+RLNS(8)
     c                   eval      aum09=aum09+RLNS(9)
     c                   eval      aum10=aum10+RLNS(10)
     c                   eval      aum11=aum11+RLNS(11)
     c                   eval      aum12=aum12+RLNS(12)
     c                   eval      aum13=aum13+RLNS(13)
     c                   eval      aum14=aum14+RLNS(14)
     c                   eval      aum15=aum15+RLNS(15)
     c                   eval      aum16=aum16+RLNS(16)
      *
     c                   exsr      cabfic
     c                   update    rorcri
     c                   exsr      Svariaz
      *
     c                   Endsr
     *****************************************************************
     *    Capi prodotti in meno su ordine bulk
     *****************************************************************
     C     capiprodm     begsr
     c
     c     korcpa        setll     avprd03k
     c                   do        *hival
     c     korcpa        reade     avprd03k
      *
     c                   if        %eof
     c                   leave
     c                   endif
      *
      * se ho avanzamenti nn saldati allora non eseguo niente e va in errore
      *
     c                   if        $icqess<>0 or $icqrie<>0 or
     c                             $icqnav<>0 or $icqprm<>0
     C                   eval      $errore = 'E'
     C                   clear                   zbferr
     C                   eval      uborab = S1ubor
     C                   eval      tdorab = S1tdor
     C                   eval      pdorab = S1pdor
     C                   eval      ndorab = S1ndor
     C                   eval      ubptab = S1ubpt
     C                   eval      protab = S1prot
     C                   eval      nrorab = S1nror
     C                   eval      msgeab = 'SNS9096'
     C                   exsr      Errore
     c                   leavesr
     c                   endif
     c                   enddo
      *
     c                   if        s1toqn>aucqta
     C                   eval      $errore = 'E'
     C                   clear                   zbferr
     C                   eval      uborab = S1ubor
     C                   eval      tdorab = S1tdor
     C                   eval      pdorab = S1pdor
     C                   eval      ndorab = S1ndor
     C                   eval      ubptab = S1ubpt
     C                   eval      protab = S1prot
     C                   eval      nrorab = S1nror
     C                   eval      msgeab = 'SNS9135'
     C                   exsr      Errore
     c                   leavesr
     c                   endif
      *
     c                   clear                   zve500
     C                   Eval      Cvargf = S1cava
     C                   Eval      varigf = S1TOQN  * (-1)
     C                   Eval      gfv01  = RLNS(1) * (-1)
     C                   Eval      gfv02  = RLNS(2) * (-1)
     C                   Eval      gfv03  = RLNS(3) * (-1)
     C                   Eval      gfv04  = RLNS(4) * (-1)
     C                   Eval      gfv05  = RLNS(5) * (-1)
     C                   Eval      gfv06  = RLNS(6) * (-1)
     C                   Eval      gfv07  = RLNS(7) * (-1)
     C                   Eval      gfv08  = RLNS(8) * (-1)
     C                   Eval      gfv09  = RLNS(9) * (-1)
     C                   Eval      gfv10  = RLNS(10) * (-1)
     C                   Eval      gfv11  = RLNS(11) * (-1)
     C                   Eval      gfv12  = RLNS(12) * (-1)
     C                   Eval      gfv13  = RLNS(13) * (-1)
     C                   Eval      gfv14  = RLNS(14) * (-1)
     C                   Eval      gfv15  = RLNS(15) * (-1)
     C                   Eval      gfv16  = RLNS(16) * (-1)
     c                   ExSr      Savorcri
     c
      *sub nelle qta in essere
      *
     c                   eval      aucqta=aucqta-S1TOQN
     c                   eval      aua01=aua01-RLNS(1)
     c                   eval      aua02=aua02-RLNS(2)
     c                   eval      aua03=aua03-RLNS(3)
     c                   eval      aua04=aua04-RLNS(4)
     c                   eval      aua05=aua05-RLNS(5)
     c                   eval      aua06=aua06-RLNS(6)
     c                   eval      aua07=aua07-RLNS(7)
     c                   eval      aua08=aua08-RLNS(8)
     c                   eval      aua09=aua09-RLNS(9)
     c                   eval      aua10=aua10-RLNS(10)
     c                   eval      aua11=aua11-RLNS(11)
     c                   eval      aua12=aua12-RLNS(12)
     c                   eval      aua13=aua13-RLNS(13)
     c                   eval      aua14=aua14-RLNS(14)
     c                   eval      aua15=aua15-RLNS(15)
     c                   eval      aua16=aua16-RLNS(16)
      *
     c                   eval      audtst=$$dat8
      *
      * sub nelle qta versate a magazzino
      *
     c                   if        aucqtm<s1toqn
     c                   eval      aucqtm=0
     c                   else
     c                   eval      aucqtm=aucqtm-S1TOQN
     c                   endif
      *
     c                   if        aum01<RLNS(1)
     c                   eval      aum01=0
     c                   else
     c                   eval      aum01=aum01-RLNS(1)
     c                   endif
      *
     c                   if        aum02<RLNS(2)
     c                   eval      aum02=0
     c                   else
     c                   eval      aum02=aum02-RLNS(2)
     c                   endif
      *
     c                   if        aum03<RLNS(3)
     c                   eval      aum03=0
     c                   else
     c                   eval      aum03=aum03-RLNS(3)
     c                   endif
      *
     c                   if        aum04<RLNS(4)
     c                   eval      aum04=0
     c                   else
     c                   eval      aum04=aum04-RLNS(4)
     c                   endif
      *
     c                   if        aum05<RLNS(5)
     c                   eval      aum05=0
     c                   else
     c                   eval      aum05=aum05-RLNS(5)
     c                   endif
      *
     c                   if        aum06<RLNS(6)
     c                   eval      aum06=0
     c                   else
     c                   eval      aum06=aum06-RLNS(6)
     c                   endif
      *
     c                   if        aum07<RLNS(7)
     c                   eval      aum07=0
     c                   else
     c                   eval      aum07=aum07-RLNS(7)
     c                   endif
      *
     c                   if        aum08<RLNS(8)
     c                   eval      aum08=0
     c                   else
     c                   eval      aum08=aum08-RLNS(8)
     c                   endif
      *
     c                   if        aum09<RLNS(9)
     c                   eval      aum09=0
     c                   else
     c                   eval      aum09=aum09-RLNS(9)
     c                   endif
      *
     c                   if        aum10<RLNS(10)
     c                   eval      aum10=0
     c                   else
     c                   eval      aum10=aum10-RLNS(10)
     c                   endif
      *
     c                   if        aum11<RLNS(11)
     c                   eval      aum11=0
     c                   else
     c                   eval      aum11=aum11-RLNS(11)
     c                   endif
      *
     c                   if        aum12<RLNS(12)
     c                   eval      aum12=0
     c                   else
     c                   eval      aum12=aum12-RLNS(12)
     c                   endif
      *
     c                   if        aum13<RLNS(13)
     c                   eval      aum13=0
     c                   else
     c                   eval      aum13=aum13-RLNS(13)
     c                   endif
      *
     c                   if        aum14<RLNS(14)
     c                   eval      aum14=0
     c                   else
     c                   eval      aum14=aum14-RLNS(14)
     c                   endif
      *
     c                   if        aum15<RLNS(15)
     c                   eval      aum15=0
     c                   else
     c                   eval      aum15=aum15-RLNS(15)
     c                   endif
      *
     c                   if        aum16<RLNS(16)
     c                   eval      aum16=0
     c                   else
     c                   eval      aum16=aum16-RLNS(16)
     c                   endif
      *
     c                   exsr      cabfic
     c                   update    rorcri
     c                   exsr      Svariaz
      *
     c                   Endsr
     *****************************************************************
     *    Cambio taglia - capo in piu' su ordine bulk
     *****************************************************************
     C     cambiotagp    begsr
      *
     c                   clear                   zve500
     C                   Eval      Cvargf = S1cava
     C                   Eval      varigf = S1TOQN
     C                   Eval      gfv01  = RLNS(1)
     C                   Eval      gfv02  = RLNS(2)
     C                   Eval      gfv03  = RLNS(3)
     C                   Eval      gfv04  = RLNS(4)
     C                   Eval      gfv05  = RLNS(5)
     C                   Eval      gfv06  = RLNS(6)
     C                   Eval      gfv07  = RLNS(7)
     C                   Eval      gfv08  = RLNS(8)
     C                   Eval      gfv09  = RLNS(9)
     C                   Eval      gfv10  = RLNS(10)
     C                   Eval      gfv11  = RLNS(11)
     C                   Eval      gfv12  = RLNS(12)
     C                   Eval      gfv13  = RLNS(13)
     C                   Eval      gfv14  = RLNS(14)
     C                   Eval      gfv15  = RLNS(15)
     C                   Eval      gfv16  = RLNS(16)
     c                   ExSr      Savorcri
     c
      *add nelle qta in essere
     c                   eval      aucqta=aucqta+S1TOQN
     c                   eval      aua01=aua01+RLNS(1)
     c                   eval      aua02=aua02+RLNS(2)
     c                   eval      aua03=aua03+RLNS(3)
     c                   eval      aua04=aua04+RLNS(4)
     c                   eval      aua05=aua05+RLNS(5)
     c                   eval      aua06=aua06+RLNS(6)
     c                   eval      aua07=aua07+RLNS(7)
     c                   eval      aua08=aua08+RLNS(8)
     c                   eval      aua09=aua09+RLNS(9)
     c                   eval      aua10=aua10+RLNS(10)
     c                   eval      aua11=aua11+RLNS(11)
     c                   eval      aua12=aua12+RLNS(12)
     c                   eval      aua13=aua13+RLNS(13)
     c                   eval      aua14=aua14+RLNS(14)
     c                   eval      aua15=aua15+RLNS(15)
     c                   eval      aua16=aua16+RLNS(16)
      *
     c                   eval      audtst=$$dat8
      *
      * add nelle qta versate a magazzino
      *
     c                   eval      aucqtm=aucqtm+S1TOQN
     c                   eval      aum01=aum01+RLNS(1)
     c                   eval      aum02=aum02+RLNS(2)
     c                   eval      aum03=aum03+RLNS(3)
     c                   eval      aum04=aum04+RLNS(4)
     c                   eval      aum05=aum05+RLNS(5)
     c                   eval      aum06=aum06+RLNS(6)
     c                   eval      aum07=aum07+RLNS(7)
     c                   eval      aum08=aum08+RLNS(8)
     c                   eval      aum09=aum09+RLNS(9)
     c                   eval      aum10=aum10+RLNS(10)
     c                   eval      aum11=aum11+RLNS(11)
     c                   eval      aum12=aum12+RLNS(12)
     c                   eval      aum13=aum13+RLNS(13)
     c                   eval      aum14=aum14+RLNS(14)
     c                   eval      aum15=aum15+RLNS(15)
     c                   eval      aum16=aum16+RLNS(16)
      *
     c                   exsr      cabfic
     c                   update    rorcri
     c                   exsr      Svariaz
      *
     c                   Endsr
     *****************************************************************
     *    Cambio taglia - capo in meno su ordine bulk
     *****************************************************************
     C     cambiotagm    begsr
      *
     c     korcpa        setll     avprd03k
     c                   do        *hival
     c     korcpa        reade     avprd03k
      *
     c                   if        %eof
     c                   leave
     c                   endif
      *
      * se ho avanzamenti nn saldati allora non eseguo niente e va in errore
      *
     c                   if        $icqess<>0 or $icqrie<>0 or
     c                             $icqnav<>0 or $icqprm<>0
     C                   eval      $errore = 'E'
     C                   clear                   zbferr
     C                   eval      uborab = S1ubor
     C                   eval      tdorab = S1tdor
     C                   eval      pdorab = S1pdor
     C                   eval      ndorab = S1ndor
     C                   eval      ubptab = S1ubpt
     C                   eval      protab = S1prot
     C                   eval      nrorab = S1nror
     C                   eval      msgeab = 'SNS9096'
     C                   exsr      Errore
     c                   leavesr
     c                   endif
     c                   enddo
     C                   if        aucqta<s1toqn   or aua01<rlns(1) or
     C                             aua02<rlns(2)   or aua03<rlns(3) or
     C                             aua04<rlns(4)   or aua05<rlns(5) or
     C                             aua06<rlns(6)   or aua07<rlns(7) or
     C                             aua08<rlns(8)   or aua09<rlns(9) or
     C                             aua10<rlns(10)  or aua11<rlns(11) or
     C                             aua12<rlns(12)  or aua13<rlns(13) or
     C                             aua14<rlns(14)  or aua15<rlns(15) or
     C                             aua16<rlns(16)
     C                   eval      $errore = 'E'
     C                   clear                   zbferr
     C                   eval      uborab = S1ubor
     C                   eval      tdorab = S1tdor
     C                   eval      pdorab = S1pdor
     C                   eval      ndorab = S1ndor
     C                   eval      ubptab = S1ubpt
     C                   eval      protab = S1prot
     C                   eval      nrorab = S1nror
     C                   eval      msgeab = 'SNS9062'
     C                   exsr      Errore
     c                   leavesr
     c                   endif
      *
     c                   clear                   zve500
     C                   Eval      Cvargf = S1cava
     C                   Eval      varigf = S1TOQN * (-1)
     C                   Eval      gfv01  = RLNS(1) * (-1)
     C                   Eval      gfv02  = RLNS(2) * (-1)
     C                   Eval      gfv03  = RLNS(3) * (-1)
     C                   Eval      gfv04  = RLNS(4) * (-1)
     C                   Eval      gfv05  = RLNS(5) * (-1)
     C                   Eval      gfv06  = RLNS(6) * (-1)
     C                   Eval      gfv07  = RLNS(7) * (-1)
     C                   Eval      gfv08  = RLNS(8) * (-1)
     C                   Eval      gfv09  = RLNS(9) * (-1)
     C                   Eval      gfv10  = RLNS(10) * (-1)
     C                   Eval      gfv11  = RLNS(11) * (-1)
     C                   Eval      gfv12  = RLNS(12) * (-1)
     C                   Eval      gfv13  = RLNS(13) * (-1)
     C                   Eval      gfv14  = RLNS(14) * (-1)
     C                   Eval      gfv15  = RLNS(15) * (-1)
     C                   Eval      gfv16  = RLNS(16) * (-1)
     c                   ExSr      Savorcri
      *
      *add nelle qta in essere
      *
     c                   eval      aucqta=aucqta-S1TOQN
     c                   eval      aua01=aua01-RLNS(1)
     c                   eval      aua02=aua02-RLNS(2)
     c                   eval      aua03=aua03-RLNS(3)
     c                   eval      aua04=aua04-RLNS(4)
     c                   eval      aua05=aua05-RLNS(5)
     c                   eval      aua06=aua06-RLNS(6)
     c                   eval      aua07=aua07-RLNS(7)
     c                   eval      aua08=aua08-RLNS(8)
     c                   eval      aua09=aua09-RLNS(9)
     c                   eval      aua10=aua10-RLNS(10)
     c                   eval      aua11=aua11-RLNS(11)
     c                   eval      aua12=aua12-RLNS(12)
     c                   eval      aua13=aua13-RLNS(13)
     c                   eval      aua14=aua14-RLNS(14)
     c                   eval      aua15=aua15-RLNS(15)
     c                   eval      aua16=aua16-RLNS(16)
      *
     c                   eval      audtst=$$dat8
      *
      * sub nelle qta versate a magazzino
      *
     c                   if        aucqtm<s1toqn
     c                   eval      aucqtm=0
     c                   else
     c                   eval      aucqtm=aucqtm-S1TOQN
     c                   endif
      *
     c                   if        aum01<RLNS(1)
     c                   eval      aum01=0
     c                   else
     c                   eval      aum01=aum01-RLNS(1)
     c                   endif
      *
     c                   if        aum02<RLNS(2)
     c                   eval      aum02=0
     c                   else
     c                   eval      aum02=aum02-RLNS(2)
     c                   endif
      *
     c                   if        aum03<RLNS(3)
     c                   eval      aum03=0
     c                   else
     c                   eval      aum03=aum03-RLNS(3)
     c                   endif
      *
     c                   if        aum04<RLNS(4)
     c                   eval      aum04=0
     c                   else
     c                   eval      aum04=aum04-RLNS(4)
     c                   endif
      *
     c                   if        aum05<RLNS(5)
     c                   eval      aum05=0
     c                   else
     c                   eval      aum05=aum05-RLNS(5)
     c                   endif
      *
     c                   if        aum06<RLNS(6)
     c                   eval      aum06=0
     c                   else
     c                   eval      aum06=aum06-RLNS(6)
     c                   endif
      *
     c                   if        aum07<RLNS(7)
     c                   eval      aum07=0
     c                   else
     c                   eval      aum07=aum07-RLNS(7)
     c                   endif
      *
     c                   if        aum08<RLNS(8)
     c                   eval      aum08=0
     c                   else
     c                   eval      aum08=aum08-RLNS(8)
     c                   endif
      *
     c                   if        aum09<RLNS(9)
     c                   eval      aum09=0
     c                   else
     c                   eval      aum09=aum09-RLNS(9)
     c                   endif
      *
     c                   if        aum10<RLNS(10)
     c                   eval      aum10=0
     c                   else
     c                   eval      aum10=aum10-RLNS(10)
     c                   endif
      *
     c                   if        aum11<RLNS(11)
     c                   eval      aum11=0
     c                   else
     c                   eval      aum11=aum11-RLNS(11)
     c                   endif
      *
     c                   if        aum12<RLNS(12)
     c                   eval      aum12=0
     c                   else
     c                   eval      aum12=aum12-RLNS(12)
     c                   endif
      *
     c                   if        aum13<RLNS(13)
     c                   eval      aum13=0
     c                   else
     c                   eval      aum13=aum13-RLNS(13)
     c                   endif
      *
     c                   if        aum14<RLNS(14)
     c                   eval      aum14=0
     c                   else
     c                   eval      aum14=aum14-RLNS(14)
     c                   endif
      *
     c                   if        aum15<RLNS(15)
     c                   eval      aum15=0
     c                   else
     c                   eval      aum15=aum15-RLNS(15)
     c                   endif
      *
     c                   if        aum16<RLNS(16)
     c                   eval      aum16=0
     c                   else
     c                   eval      aum16=aum16-RLNS(16)
     c                   endif
      *
     c                   exsr      cabfic
     c                   update    rorcri
     c                   exsr      Svariaz
      *
     c                   Endsr
     *****************************************************************
     *    Cambio colore - capo in piu 067
     *****************************************************************
     C     cambiocolo    begsr
      *
     c                   clear                   z@cblk
     c                   eval      $bulk=*blank
     c                   eval      $volta=*off
      *
     c     'BLK'         setll     tsian99k
     c                   do        *hival
     c     'BLK'         reade     tsian99k
     c                   if        %eof
     c                   leave
     c                   endif
     c                   eval      z@cblk=A2XTAB
     c                   if        UDMSIM=S1DBOW  AND MAGSIM=S1MAGA
     c                   eval      $bulk=a2cele
      *
     c                   movel     s1car1        ctec
     c                   movel     s1car2        cmod
     c                   movel     s1car3        colc

     c     korcrim       setll     ORCRI0MK
     c                   do        *hival
     c     korcrim       reade     ORCRI0MK
      *
     c                   if        %eof
     c                   leave
     c                   endif
      *
     c                   if        m_austat='BO' and
     c                             $bulk=m_aucblk and m_aucblk<>*blank
     c                             and
     C                             m_aucqta>=s1toqn  and m_aua01>=rlns(1)  and
     C                             m_aua02>=rlns(2)  and m_aua03>=rlns(3)  and
     C                             m_aua04>=rlns(4)  and m_aua05>=rlns(5)  and
     C                             m_aua06>=rlns(6)  and m_aua07>=rlns(7)  and
     C                             m_aua08>=rlns(8)  and m_aua09>=rlns(9)  and
     C                             m_aua10>=rlns(10) and m_aua11>=rlns(11) and
     C                             m_aua12>=rlns(12) and m_aua13>=rlns(13) and
     C                             m_aua14>=rlns(14) and m_aua15>=rlns(15) and
     C                             m_aua16>=rlns(16)
     c                   clear                   zve500
     C                   Eval      Cvargf = S1cava
     C                   Eval      varigf = S1TOQN
     C                   Eval      gfv01  = RLNS(1)
     C                   Eval      gfv02  = RLNS(2)
     C                   Eval      gfv03  = RLNS(3)
     C                   Eval      gfv04  = RLNS(4)
     C                   Eval      gfv05  = RLNS(5)
     C                   Eval      gfv06  = RLNS(6)
     C                   Eval      gfv07  = RLNS(7)
     C                   Eval      gfv08  = RLNS(8)
     C                   Eval      gfv09  = RLNS(9)
     C                   Eval      gfv10  = RLNS(10)
     C                   Eval      gfv11  = RLNS(11)
     C                   Eval      gfv12  = RLNS(12)
     C                   Eval      gfv13  = RLNS(13)
     C                   Eval      gfv14  = RLNS(14)
     C                   Eval      gfv15  = RLNS(15)
     C                   Eval      gfv16  = RLNS(16)
     c                   ExSr      Savorcrim
     c
      *add nelle qta in essere
     c                   eval      m_aucqrv=m_aucqrv+S1TOQN
     c                   eval      m_aur01=m_aur01+RLNS(1)
     c                   eval      m_aur02=m_aur02+RLNS(2)
     c                   eval      m_aur03=m_aur03+RLNS(3)
     c                   eval      m_aur04=m_aur04+RLNS(4)
     c                   eval      m_aur05=m_aur05+RLNS(5)
     c                   eval      m_aur06=m_aur06+RLNS(6)
     c                   eval      m_aur07=m_aur07+RLNS(7)
     c                   eval      m_aur08=m_aur08+RLNS(8)
     c                   eval      m_aur09=m_aur09+RLNS(9)
     c                   eval      m_aur10=m_aur10+RLNS(10)
     c                   eval      m_aur11=m_aur11+RLNS(11)
     c                   eval      m_aur12=m_aur12+RLNS(12)
     c                   eval      m_aur13=m_aur13+RLNS(13)
     c                   eval      m_aur14=m_aur14+RLNS(14)
     c                   eval      m_aur15=m_aur15+RLNS(15)
     c                   eval      m_aur16=m_aur16+RLNS(16)
      *
     c                   eval      m_audtst=$$dat8
      *
     c                   exsr      cabfic
     c                   update    orcri0m
     c                   exsr      Svariaz
     c                   eval      $volta=*on
     c                   leave
     c                   endif
      *
     c                   enddo
     c                   leave
     c                   endif
     c                   enddo
      *
     c                   if        $volta=*off
     C                   eval      $errore = 'E'
     C                   clear                   zbferr
     C                   eval      uborab = S1ubor
     C                   eval      tdorab = S1tdor
     C                   eval      pdorab = S1pdor
     C                   eval      ndorab = S1ndor
     C                   eval      ubptab = S1ubpt
     C                   eval      protab = S1prot
     C                   eval      nrorab = S1nror
     C                   eval      msgeab = 'CSI0001'
     C                   exsr      Errore
     c                   endif
      *
      *
     c                   Endsr
     ************************************************************************************
     *    Assegnazione: assegno in aggiunta a ordine BO versato per attribuzione da vasca
     ************************************************************************************
     C     asse_vasca    begsr
      *
     c                   clear                   zve500
     C                   Eval      Cvargf = S1cava
     C                   Eval      varigf = S1TOQN * (1)
     C                   Eval      gfv01  = RLNS(1)  * (1)
     C                   Eval      gfv02  = RLNS(2)  * (1)
     C                   Eval      gfv03  = RLNS(3)  * (1)
     C                   Eval      gfv04  = RLNS(4)  * (1)
     C                   Eval      gfv05  = RLNS(5)  * (1)
     C                   Eval      gfv06  = RLNS(6)  * (1)
     C                   Eval      gfv07  = RLNS(7)  * (1)
     C                   Eval      gfv08  = RLNS(8)  * (1)
     C                   Eval      gfv09  = RLNS(9)  * (1)
     C                   Eval      gfv10  = RLNS(10) * (1)
     C                   Eval      gfv11  = RLNS(11) * (1)
     C                   Eval      gfv12  = RLNS(12) * (1)
     C                   Eval      gfv13  = RLNS(13) * (-1)
     C                   Eval      gfv14  = RLNS(14) * (-1)
     C                   Eval      gfv15  = RLNS(15) * (-1)
     C                   Eval      gfv16  = RLNS(16) * (-1)
     c                   ExSr      Savorcri
      *
     c                   eval      audtst=$$dat8
      *
     C                   if        aucqtm=s1toqn  or aum01+rlns(1)>aua01 or
     C                             aum02+rlns(2)>aua02 or aum03+rlns(3)>aua03 or
     C                             aum04+rlns(4)>aua04 or aum05+rlns(5)>aua05 or
     C                             aum06+rlns(6)>aua06 or aum07+rlns(7)>aua07 or
     C                             aum08+rlns(8)>aua08 or aum09+rlns(9)>aua09 or
     C                             aum10+rlns(10)>aua10 or aum11+rlns(11)>aua11
     C                             or aum12+rlns(12)>aua12 or
     C                             aum13+rlns(13)>aua13 or aum14+rlns(14)>aua14
     C                             or aum14+rlns(14)>aua14 or
     C                             aum15+rlns(15)>aua15 or aum16+rlns(16)>aua16
     C                   eval      $errore = 'E'
     C                   clear                   zbferr
     C                   eval      uborab = S1ubor
     C                   eval      tdorab = S1tdor
     C                   eval      pdorab = S1pdor
     C                   eval      ndorab = S1ndor
     C                   eval      ubptab = S1ubpt
     C                   eval      protab = S1prot
     C                   eval      nrorab = S1nror
     C                   eval      msgeab = 'SNS9931'
     C                   exsr      Errore
     c                   leavesr
     c                   endif
     c                   eval      aucqtm=aucqtm-S1TOQN
     c                   eval      aum01=aum01-RLNS(1)
     c                   eval      aum02=aum02-RLNS(2)
     c                   eval      aum03=aum03-RLNS(3)
     c                   eval      aum04=aum04-RLNS(4)
     c                   eval      aum05=aum05-RLNS(5)
     c                   eval      aum06=aum06-RLNS(6)
     c                   eval      aum07=aum07-RLNS(7)
     c                   eval      aum08=aum08-RLNS(8)
     c                   eval      aum09=aum09-RLNS(9)
     c                   eval      aum10=aum10-RLNS(10)
     c                   eval      aum11=aum11-RLNS(11)
     c                   eval      aum12=aum12-RLNS(12)
     c                   eval      aum13=aum13-RLNS(13)
     c                   eval      aum14=aum14-RLNS(14)
     c                   eval      aum15=aum15-RLNS(15)
     c                   eval      aum16=aum16-RLNS(16)
     c                   exsr      cabfic
     c                   update    rorcri
     c                   exsr      Svariaz
      *
     c                   Endsr
     *****************************************************************
     *    Set_Data : imposto la data con la data odierna, ma se forzata
     *               da CSIS come data di carico magazzino utilizzo
     *               Quest'ultima.
     *****************************************************************
     C     Set_Data      begsr
     *
     * Imposto la XXDAT8 con la data odierna in formato AAAAMMGG
     C***                Eval      $dat06 = udate
     * da data di sistema:
moss c**                 EVAL      CMD = 'CHGJOB DATFMT(*DMY) '
     C**                 CALL(E)   'QCMDEXC'
     C**                 PARM                    CMD
     C**                 PARM      80            LUN              15 5
dla  C**                 TIME                    TimStp           12 0
dla  C**                 Move      TimStp        $dat06
     C                   Eval      $dat08 = 0
     C                   Eval      $dat06 = %dec(%date():*dmy)
     C                   Eval      $modo = 'DY'
     C                   Exsr      Girodata
     C                   Eval      $$dat8 = xxdat8
     *
     * Verifico se devo forzare la data secondo quanto caricato da CSIS
     * a magazzino in formato AAAAMMGG.
     c                   IF        s1anpr>0 and (s1stnw ='MAG' or
     C                             s1stnw = 'CLD' or S1stnw = 'RIC')
     *
     c                   z-add     s1anpr        $data1
     C                   Eval      $err_chk= *OFF
     *
     C                   Monitor
     C                   MOVEL     $data1        dataiso
     C                   On-Error
     C                   Eval      $err_chk= *ON
     C                   EndMon
     *
     C                   if        $err_chk=*off And $data1 <> 0
     c                   eval      $$dat8=$data1
     *
     C                   Endif
     C                   EndIf
     *
     * Setto la data XXDAT8 in formato AAAAMMGG e la $$GGMMAAAA in formato GGMMAAAA
     C                   Eval      $dat06 = 0
     C                   Eval      $dat08 = $$DAT8
     C                   Eval      $modo = 'YD'
     C                   Exsr      Girodata
     C                   Eval      $$GGMMAAAA = xxdat8
     *
moss c                   EVAL      CMD = 'CHGJOB DATE(' +%EDITC(xxdat6:'X')+
moss c                             ')'
     C                   CALL(E)   'QCMDEXC'
     C                   PARM                    CMD
     C                   PARM      80            LUN              15 5
     *
     C                   IF        (%error)
     C                   eval      xxdat6 = %dec(%date(xxdat6:*dmy):*mdy)
moss c                   EVAL      CMD = 'CHGJOB DATE(' +%EDITC(xxdat6:'X')+
moss c                             ')'
     C                   CALL(E)   'QCMDEXC'
     C                   PARM                    CMD
     C                   PARM      80            LUN              15 5
     C                   EndIf
     *
     C                   endsr
     *****************************************************************
     *    Ctr_colv     : controllo se la fase succesiva e' di collaudo
     *                   virtuale
     *****************************************************************
     C     ctr_colv      begsr
     c
      * ctrl se esiste fase
     C                   eval      colvir=*blank
 "   C                   Clear                   z@cfpd
 "   C                   Movel(P)  'FPD'         $ctab
 "   C                   Movel(P)  S1UBUS        $cele
 "   C                   Exsr      Ctl_decotab
 "   C                   If        @DLI3 <> '1'
 "   C                   Movel(P)  @dli2         z@cfpd
     C                   if        fcolib='Y' and  fvirib='Y'
     c                   eval      colvir='Y'
     c                   endif
     c                   else
     c                   eval      colvir='N'
 "   C                   EndIf
     C
     C                   endsr
     C
     *****************************************************************
     *    Ann_no_ORCR  : Se mi arriva una richiesta di annullamento e
     *                   non trovo ORCRI, flaggo l'avanzamento come
     *                   sospeso e non elaboro.
     *****************************************************************
     C     Ann_no_ORCR   begsr
     *
     C                   if        s1stnw ='ANN' and S1tdor <> *blanks
     C                             and s1stol = '   '
     C     Kriga         chain     orcri28k
     C                   if        not %found
     C     Krigap        chain     orcri28k
     C                   if        not %found
     C                   eval      S1flan = 'S'
     C                   eval      $ITER = 'Y'
PG1  c                   exsr      Rip_ord_box
     C                   update    SNS01
     C                   endif
     C                   endif
     *
     C                   endif
     *
     C                   endsr
     *
     *****************************************************************
     *    Preannulla    : Richiamo modulo che
     *                    mi scrive gli archivi ORCPA o ORCPP
     *                    ed allineo ORPRA e ORCRI
     *****************************************************************
     C     Preannulla    begsr
     *
     C     Korcpa        chain(N)  orpra55k
     C                   IF        austat ='BO' and not %FOUND(orpra55k)
     C                   eval      $iter = 'Y'
     C                   leavesr
     C                   endif
     *
     C                   Exsr      Controlli_ann
     C                   if        $errore <> *blanks
     C                             or $controllo = *OFF
     C                   leavesr
     C                   endif
     *
     * Allineo ORPRA
     C                   eval      e6nopr = aunord
     C                   eval      e6ropr = aunrig
     C                   eval      e6srpr = ausrig
     C                   clear                   $cont
     C                   if        $ccpa <> *blanks
     C     Korpra        setll     orpra55k
     C                   else
     C     KorpraSNS     setll     orpra55k
     C                   endif
     C                   do        *hival
     C                   if        $ccpa <> *blanks
     C     Korpra        reade     orpra55k
     C                   else
     C     KorpraSNS     reade     orpra55k
     C                   endif
     C                   if        %EOF
     C                   leave
     C                   endif
     *
     * Verifico se e' un componente della parte di capo, se si la scarto
     C     e6ccpa        chain     cpman01k
     c                   IF        %found(cpman01k)
     c                             and acfgcm = 'Y'
     c                   iter
     c                   endif
     *
     C                   eval      $cont = $cont + 1
     C                   exsr      Verifica_prea
     C                   if        qtaafy <> *zeros
     C                   exsr      allineo_qta
BET  C                   ExSr      Allinea_fasi
     C                   endif
     C                   enddo
     *
     * Allineo ORCRI
     C                   if        qtaafy <> *zeros
     C                             and e6ccpa = auccpm
     c                   clear                   ZVE500
     c                   eval      ANMCGF = 'Y'
     C                   Eval      Cvargf = $cvar
     C                   Eval      varigf = qtaafy * (-1)
     C                   Eval      gfv01  = afy01  * (-1)
     C                   Eval      gfv02  = afy02  * (-1)
     C                   Eval      gfv03  = afy03  * (-1)
     C                   Eval      gfv04  = afy04  * (-1)
     C                   Eval      gfv05  = afy05  * (-1)
     C                   Eval      gfv06  = afy06  * (-1)
     C                   Eval      gfv07  = afy07  * (-1)
     C                   Eval      gfv08  = afy08  * (-1)
     C                   Eval      gfv09  = afy09  * (-1)
     C                   Eval      gfv10  = afy10  * (-1)
     C                   Eval      gfv11  = afy11  * (-1)
     C                   Eval      gfv12  = afy12  * (-1)
     C                   Eval      gfv13  = afy13  * (-1)
     C                   Eval      gfv14  = afy14  * (-1)
     C                   Eval      gfv15  = afy15  * (-1)
     C                   Eval      gfv16  = afy16  * (-1)
 "   C                   ExSr      Savorcri
     C                   exsr      Aggio_qtaorcri
     C                   exsr      allineo_orcri
 "   c                   exsr      Svariaz
     C                   exsr      Mod_ann_blk
     C                   endif
     *
     C                   endsr
     *
     *****************************************************************
     *    Controlli_ann : Se sto preannullando i capi,
     *                    verifico che i dati passatomi siano
     *                    compilati correttamente
     *****************************************************************
     C     Controlli_ann begsr
     *
     * Se ordine CSIS non passato non posso preannullare i capi
     C                   if        S1ubor = *blanks
     C                   leavesr
     C                   else
     C                   eval      $controllo = *ON
     C                   endif
     * Trascodifico la causale variazione (CVAR)
     C                   clear                   $cvar
     C*                  Movel(P)  'VAR'         $ctab
     C*                  Movel(P)  S1cava        $cele
     C*                  Exsr      Trasco_tabe
     C*                  if        @tsc2 <> *blanks
     C*                  movel     @tsc2         $cvar
     C*                  else
     C*                  eval      $errore = 'E'
     C*                  clear                   zbferr
     C*                  eval      uborab = S1ubor
     C*                  eval      tdorab = S1tdor
     C*                  eval      pdorab = S1pdor
     C*                  eval      ndorab = S1ndor
     C*                  eval      ubptab = S1ubpt
     C*                  eval      protab = S1prot
     C*                  eval      nrorab = S1nror
     C*                  eval      cavaab = S1cava
     C*                  eval      msgeab = 'SNS7588'
     C*                  exsr      Errore
     C*                  leavesr
     C*                  endif
     *
     * Verifico che la causale sia tabellata in SNS
     C                   movel     S1cava        $cvar
     C                   Movel(P)  'VAR'         $ctab
     C                   Movel(P)  $cvar         $cele
     C                   Exsr      Ctl_decotab
     C                   If        @DLI3 = '1'
     C                   eval      $errore = 'E'
     C                   clear                   zbferr
     C                   eval      uborab = S1ubor
     C                   eval      tdorab = S1tdor
     C                   eval      pdorab = S1pdor
     C                   eval      ndorab = S1ndor
     C                   eval      ubptab = S1ubpt
     C                   eval      protab = S1prot
     C                   eval      nrorab = S1nror
     C                   eval      cvarab = $cvar
     C                   eval      msgeab = 'SNS3498'
     C                   exsr      Errore
     C                   leavesr
     C                   Else
     C                   Movel(P)  @dli2         z@cvar
     C                   endif
     *
     * Trascodifico la qualita (QLT)
     C                   if        S1tspr  <> *blanks
     C                   clear                   $qual
     C                   Movel(P)  'QLT'         $ctab
     C                   Movel(P)  S1tspr        $cele
     C                   Exsr      Trasco_tabe
     C                   if        @tsc2 <> *blanks
     C                   movel     @tsc2         $qual
     C                   else
     C                   eval      $errore = 'E'
     C                   clear                   zbferr
     C                   eval      uborab = S1ubor
     C                   eval      tdorab = S1tdor
     C                   eval      pdorab = S1pdor
     C                   eval      ndorab = S1ndor
     C                   eval      ubptab = S1ubpt
     C                   eval      protab = S1prot
     C                   eval      nrorab = S1nror
     C                   eval      tsprab = S1tspr
     C                   eval      msgeab = 'SNS7588'
     C                   exsr      Errore
     C                   leavesr
     C                   endif
     * Verifico che la causale sia tabellata in SNS
     C                   Movel(P)  'QLT'         $ctab
     C                   Movel(P)  $qual         $cele
     C                   Exsr      Ctl_decotab
     C                   If        @DLI3 = '1'
     C                   eval      $errore = 'E'
     C                   clear                   zbferr
     C                   eval      uborab = S1ubor
     C                   eval      tdorab = S1tdor
     C                   eval      pdorab = S1pdor
     C                   eval      ndorab = S1ndor
     C                   eval      ubptab = S1ubpt
     C                   eval      protab = S1prot
     C                   eval      nrorab = S1nror
     C                   eval      qualab = $qual
     C                   eval      msgeab = 'SNS4816'
     C                   exsr      Errore
     C                   leavesr
     C                   endif
     C                   endif
     *
     C                   endsr
     *
     **********************************************************************
     *    Magazzino   : Ricezione da trasferimento SNS-CSIS con status WM
     *                  La riga non pu¦ essere BLK xk} arriva da premag SNS
     **********************************************************************
     C     Magazzino     begsr
     *
     * Verifico status di ORCRI
     C**$                if        austan <> '190'
     C                   if        austan <  '190'
     C                   eval      $errore = 'E'
     C                   clear                   zbferr
     C                   eval      uborab = S1ubor
     C                   eval      tdorab = S1tdor
     C                   eval      pdorab = S1pdor
     C                   eval      ndorab = S1ndor
     C                   eval      ubptab = S1ubpt
     C                   eval      protab = S1prot
     C                   eval      nrorab = S1nror
     C                   eval      cstgab = aucstg
     C                   eval      cudbab = aucudb
     C                   eval      ccliab = auccli
     C                   eval      nordab = aunord
     C                   eval      nrigab = aunrig
     C                   eval      srigab = ausrig
     C                   eval      noprab = e6nopr
     C                   eval      roprab = e6ropr
     C                   eval      srprab = e6srpr
     C                   eval      ccpaab = $ccpa
     C                   eval      statab = austat
     C                   eval      msgeab = 'SNS2725'
     C                   exsr      Errore
     C                   leavesr
     C                   endif
     *--Aggiorno ORCRI
BET  C                   exsr      Aggio_qtaorcri
     C                   if        austan =  '190'
     C                   eval      audtst = $$dat8
     C                   eval      austat = 'MA'
     C                   endif
     C     austat        chain     evean01k
     C                   if        %found
     C                   eval      austan = cgstan
     C                   else
     C                   eval      $errore = 'E'
     C                   clear                   zbferr
     C                   eval      uborab = S1ubor
     C                   eval      tdorab = S1tdor
     C                   eval      pdorab = S1pdor
     C                   eval      ndorab = S1ndor
     C                   eval      ubptab = S1ubpt
     C                   eval      protab = S1prot
     C                   eval      nrorab = S1nror
     C                   eval      cstgab = aucstg
     C                   eval      cudbab = aucudb
     C                   eval      ccliab = auccli
     C                   eval      nordab = aunord
     C                   eval      nrigab = aunrig
     C                   eval      srigab = ausrig
     C                   eval      noprab = e6nopr
     C                   eval      roprab = e6ropr
     C                   eval      srprab = e6srpr
     C                   eval      ccpaab = $ccpa
     C                   eval      statab = austat
     C                   eval      msgeab = 'SNS2725'
     C                   exsr      Errore
     C                   leavesr
     C                   endif
     *
     C                   exsr      cabfic
     C                   update    rorcri
     *
     C                   eval      e6nopr = aunord
     C                   eval      e6ropr = aunrig
     C                   eval      e6srpr = ausrig
     C     KorpraSNS     setll     orpra55k
     C                   do        *hival
     C     KorpraSNS     reade     orpra55k
     C                   if        %EOF
     C                   leave
     C                   endif
     *
     * Verifico se e' un componente della parte di capo, se si la scarto
     C     e6ccpa        chain     cpman01k
     c                   IF        %found(cpman01k)
     c                             and acfgcm = 'Y'
     c                   iter
     c                   endif
     *
     * Verifico status di ORPRA
     C**$                if        e6stan <> '190'
     C                   if        e6stan <  '190'
     C                   eval      $errore = 'E'
     C                   clear                   zbferr
     C                   eval      uborab = S1ubor
     C                   eval      tdorab = S1tdor
     C                   eval      pdorab = S1pdor
     C                   eval      ndorab = S1ndor
     C                   eval      ubptab = S1ubpt
     C                   eval      protab = S1prot
     C                   eval      nrorab = S1nror
     C                   eval      cstgab = aucstg
     C                   eval      cudbab = aucudb
     C                   eval      ccliab = auccli
     C                   eval      nordab = aunord
     C                   eval      nrigab = aunrig
     C                   eval      srigab = ausrig
     C                   eval      noprab = e6nopr
     C                   eval      roprab = e6ropr
     C                   eval      srprab = e6srpr
     C                   eval      ccpaab = $ccpa
     C                   eval      stapab = e6stat
     C                   eval      msgeab = 'SNS2725'
     C                   exsr      Errore
     C                   leaveSR
     C                   endif
     *
     * Verifico magazzino
     C                   If        e6stan =  '190'
     C     e6masm        chain     magan02k
     C                   if        %found
     C                             and a003bz <> ##cudb
     C                   eval      $errore = 'E'
     C                   clear                   zbferr
     C                   eval      uborab = S1ubor
     C                   eval      tdorab = S1tdor
     C                   eval      pdorab = S1pdor
     C                   eval      ndorab = S1ndor
     C                   eval      ubptab = S1ubpt
     C                   eval      protab = S1prot
     C                   eval      nrorab = S1nror
     C                   eval      cstgab = aucstg
     C                   eval      cudbab = aucudb
     C                   eval      ccliab = auccli
     C                   eval      nordab = aunord
     C                   eval      nrigab = aunrig
     C                   eval      srigab = ausrig
     C                   eval      noprab = e6nopr
     C                   eval      roprab = e6ropr
     C                   eval      srprab = e6srpr
     C                   eval      ccpaab = $ccpa
     C                   eval      magsab = e6masm
     C                   eval      msgeab = 'SNS3486'
     C                   exsr      Errore
     C                   leavesr
     C                   endif
     C                   Endif
     *
     * Aggiorno ORPRA con lo status SNS di magazzino
     C                   if        e6stan =  '190'
     C                   eval      e6stat = 'SD'
     C                   endif
     C     e6stat        chain     evean01k
     C                   if        %found
     C                   eval      e6stan = cgstan
     C                   else
     C                   eval      $errore = 'E'
     C                   clear                   zbferr
     C                   eval      uborab = S1ubor
     C                   eval      tdorab = S1tdor
     C                   eval      pdorab = S1pdor
     C                   eval      ndorab = S1ndor
     C                   eval      ubptab = S1ubpt
     C                   eval      protab = S1prot
     C                   eval      nrorab = S1nror
     C                   eval      cstgab = aucstg
     C                   eval      cudbab = aucudb
     C                   eval      ccliab = auccli
     C                   eval      nordab = aunord
     C                   eval      nrigab = aunrig
     C                   eval      srigab = ausrig
     C                   eval      noprab = e6nopr
     C                   eval      roprab = e6ropr
     C                   eval      srprab = e6srpr
     C                   eval      ccpaab = $ccpa
     C                   eval      stapab = e6stat
     C                   eval      msgeab = 'SNS8148'
     C                   exsr      Errore
     C                   leavesr
     C                   endif
     *
     C                   exsr      cabfic
     C                   update    rorpra
     C                   enddo
     *
     *  Scarico magazzino di transito SNS
     C                   if        $errore = *blanks
     C                   Exsr      Scarico_Mag
     C                   endif
     *
     C                   endsr
     *
     *****************************************************************
     *    Crea_box: Crea gli archivi box SNS: S14TE, S14RI, SSCCB
     *
     *****************************************************************
     C     Crea_box      begsr
     *
     c                   leavesr
      *
     C     S1ubus        chain     udban03k
     C                   if        %found(Udban03k)
     C                             and apcudb = ##cudb
     C                   else
     C                   eval      $errore = 'E'
     C                   clear                   zbferr
     C                   eval      ubusab = S1ubus
     C                   eval      tdocab = S1tdoc
     C                   eval      pdocab = S1pdoc
     C                   eval      ndocab = S1ndoc
     C                   eval      ubusab = S1ubus
     C                   eval      msgeab = 'SNS8346'
     C                   exsr      Errore
     c                   leavesr
     C                   endif
     *
     C                   eval      $crearighe = 'N'
     C     Kfile         chain     Bovte51l
     C                   if        %found
     C                   exsr      Testata_box
     C                   if        $errore = *blanks
     C                   eval      $crearighe = 'Y'
     C                   endif
     C                   else
     C                   eval      $errore = 'E'
     C                   clear                   zbferr
     C                   eval      ubusab = S1ubus
     C                   eval      tdocab = S1tdoc
     C                   eval      pdocab = S1pdoc
     C                   eval      ndocab = S1ndoc
     C                   eval      msgeab = 'SNS8150'
     C                   exsr      Errore
     C                   endif
     *
     C                   if        $crearighe = 'Y'
     C     Kfile         chain     Bovda51l
     C                   if        not %found
     C                   eval      $errore = 'E'
     C                   clear                   zbferr
     C                   eval      ubusab = S1ubus
     C                   eval      tdocab = S1tdoc
     C                   eval      pdocab = S1pdoc
     C                   eval      ndocab = S1ndoc
     C                   eval      msgeab = 'SNS8151'
     C                   exsr      Errore
     C                   else
     C                   exsr      Wrt_SSCCB
     C                   endif
     *
     C     Kfile         setll     Bovrg51l
     C                   do        *hival
     C     Kfile         reade     Bovrg51l
     C                   if        %EOF
     C                   leave
     C                   endif
     *
     C                   exsr      Righe_box
     C                   if        $iter = 'Y'
     C                   leave
     C                   endif
     C                   eval      gocapi = gocapi + gpcapi
     C                   eval      a003gp = ##cudb
     C                   eval      a004gp = ##cudb
     C                   eval      a005gp = ##cudb
     C                   exsr      cabfic
     C                   write     rs14ri
     C                   enddo
     C                   if        $iter = 'Y'
     C                   leavesr
     C                   endif
     C                   eval      gocfam = gpcfam
     C                   eval      gofric = 'Y'
     C                   eval      a003go = ##cudb
     C                   eval      a004go = ##cudb
     C                   eval      a005go = ##cudb
     C                   write     rs14te
     C                   endif
     *
     C                   endsr
     *
     *****************************************************************
     *    Annullo_box: Cancello archivi box SNS: S14TE, S14RI, SSCCB
     *
     *****************************************************************
     C     Annullo_box   begsr
     *
     *
     C     S1ubus        chain     udban03k
     C                   if        %found(Udban03k)
     c                   eval      goudbb = apcudb
     C                   else
     C                   eval      $errore = 'E'
     C                   clear                   zbferr
     C                   eval      ubusab = S1ubus
     C                   eval      tdocab = S1tdoc
     C                   eval      pdocab = S1pdoc
     C                   eval      ndocab = S1ndoc
     C                   eval      ubusab = S1ubus
     C                   eval      msgeab = 'SNS7166'
     C                   exsr      Errore
     c                   leavesr
     C                   endif
     C                   movel     S1pdoc        $anno
     C                   move      $anno         goannb
     C                   clear                   $numerobox
     C                   eval      $numerobox = S1ndoc
     c                   move      $anno         goannb
     C                   move      $seriebox     gosbox
     c                   move      $numbox       gonumb
     *
     C     KS14te        chain     S14te01k
     C                   if        not %found(S14te01k)
     C                   eval      $errore = 'E'
     C                   clear                   zbferr
     C                   eval      udbbab = apcudb
     C                   eval      annbab = goannb
     C                   eval      serbab = $seriebox
     C                   eval      numbab = gonumb
     C                   eval      msgeab = 'SNS8295'
     C                   exsr      Errore
     C                   Leavesr
     C                   else
     C                   eval      $controllo = *ON
     C                   endif

     C                   eval      v5numb = gonumb
     C     KSSCCB        chain     SSCCB01k
     C                   if        %found(SSCCB01k)
     C                   eval      a001v5 = 'A'
     C                   exsr      cabfic
     C                   update    SSCCB01
     C                   endif
     *
     C     Ks14te        setll     S14ri01k
     C                   do        *hival
     c     Ks14TE        reade     S14ri01k
     c                   if        %EOF
     C                   eval      a001go = 'A'
     C                   exsr      cabfic
     C                   update    S14TE01
     c                   leave
     c                   endif
     C     Korcri        chain     Orcri01k
     C                   if        %found(Orcri01k)
     C                   eval      auflbx = 'N'
     c                   eval      auctsp = 'APP'
     C                   eval      aupubo = *blanks
     C                   eval      auptdo = *blanks
     C                   eval      auppdo = *blanks
     C                   eval      aupndo = *blanks
     C                   if        austat = 'SL'
     C                   eval      audtst = $$dat8
     C                   eval      austat = 'MA'
     C     austat        chain     evean01k
     C                   if        %found
     C                   eval      austan = cgstan
     C                   endif
     C                   endif
     C                   exsr      cabfic
     C                   update    ORCRI01
     C                   endif
     C                   eval      a001gp = 'A'
     C                   exsr      cabfic
     c                   update    S14ri01
     c                   enddo
     *
     C                   endsr
     *
     *****************************************************************
     *    Proposta: Controlli per aggiornare la proposta CSIS
     *****************************************************************
     C     Proposta      begsr
     *
     C     Kfile         chain     Lprte51l
     C                   if        not %found
     C                   eval      $errore = 'E'
     C                   clear                   zbferr
     C                   eval      ubusab = S1ubus
     C                   eval      tdocab = S1tdoc
     C                   eval      pdocab = S1pdoc
     C                   eval      ndocab = S1ndoc
     C                   eval      msgeab = 'SNS8152'
     C                   exsr      Errore
     C                   leavesr
     C                   endif
     *
     C     Kfile         setll     Lprrg51l
     C                   do        *hival
     C     Kfile         reade     Lprrg51l
     C                   if        %EOF
     C                   leave
     C                   endif
     *
     C                   eval      $rigalis = lrnrlp
     *
     C     Kovdrglis     chain     ovdrg51l
     C                   if        not %found
     C                   eval      $errore = 'E'
     C                   clear                   zbferr
     C                   eval      uborab = lrubor
     C                   eval      tdorab = lrtdor
     C                   eval      pdorab = lrpdor
     C                   eval      ndorab = lrndor
     C                   eval      nrorab = lrnror
     C                   eval      msgeab = 'SNS8153'
     C                   exsr      Errore
     C                   leavesr
     C                   endif
     *
     C                   eval      rigaord = lrnror
     c                   clear                   $numcpa
     *
     *-- Verifico se } un cambio cliente
     C                   eval      $ubor  = lrubor
     C                   eval      $tdor  = lrtdor
     C                   eval      $pdor  = lrpdor
     C                   eval      $ndor  = lrndor
     C                   eval      $nror  = lrnror
     C                   eval      $iter = 'N'
     C                   exsr      Test_Oadrg
     C                   if        $iter = 'Y'
     C                   eval      $iter = 'N'
     C                   iter
     C*PIER              eval      $errore = 'S'
     C*                  leavesr
     C                   endif
     *
     C     Kor28lis      chain     orcri28k
     C                   if        not %found
     C***                eval      $errore = 'S'
     C**                 clear                   zbferr
     C***                eval      uborab = lrubor
     C***                eval      tdorab = lrtdor
     C**                 eval      pdorab = lrpdor
     C***                eval      ndorab = lrndor
     C***                eval      nrorab = lrnror
     C***                eval      msgeab = 'SNS8145'
     C***                exsr      Errore
     C***                leavesr
     C                   iter
     C                   endif
     *
     C     aumags        chain     magan02k
     C                   if        a003bz <> ##cudb and
     c                             (s1stol<>'ASO' or S1STNW<>'ANN')              berizzi 21102013
     C                   eval      $iter = 'Y'
     C                   eval      $errore = 'Y'
     C                   leavesr
     C                   endif
     *
     * Verifico lo status di ORCRI se non } un BULK
     C                   If        aucblk = '   ' and
     C***                          austat <> 'RM ' and Austat <> 'MA'
     C                             austan < '200'
     C                   eval      $errore = 'E'
     C                   clear                   zbferr
     C                   eval      uborab = lrubor
     C                   eval      tdorab = lrtdor
     C                   eval      pdorab = lrpdor
     C                   eval      ndorab = lrndor
     C                   eval      nrorab = lrnror
     C                   eval      cstgab = aucstg
     C                   eval      cudbab = aucudb
     C                   eval      ccliab = auccli
     C                   eval      nordab = aunord
     C                   eval      nrigab = aunrig
     C                   eval      srigab = ausrig
     C                   eval      ccpaab = $ccpa
     C                   eval      statab = austat
     C                   eval      msgeab = 'SNS2725'
     C                   exsr      Errore
     C                   leavesr
     C                   endif
     *
     C                   If        aucblk = '   ' and
     C                             austan > '205'
     C                   iter
     C                   endif
     *
     C                   eval      aupubo = lrubpr
     C                   eval      auptdo = lrtdlp
     C                   eval      auppdo = lrpdpr
     C                   eval      aupndo = lrndlp
     C                   eval      audtpr = tpdlpr
     *
     C                   if        aucblk = *blanks
     C                   eval      audtst = $$dat8
     C                   eval      audtsp = $$dat8
     C                   eval      austat = 'SL'
     C     austat        chain     evean01k
     C                   if        %found
     C                   eval      austan = cgstan
     C                   else
     C                   eval      $errore = 'E'
     C                   clear                   zbferr
     C                   eval      uborab = au$ubo
     C                   eval      tdorab = au$tdo
     C                   eval      pdorab = au$pdo
     C                   eval      ndorab = au$ndo
     C                   eval      ubptab = au$ubp
     C                   eval      protab = au$pro
     C                   eval      nrorab = au$nro
     C                   eval      cstgab = aucstg
     C                   eval      cudbab = aucudb
     C                   eval      ccliab = auccli
     C                   eval      nordab = aunord
     C                   eval      nrigab = aunrig
     C                   eval      srigab = ausrig
     C                   eval      ccpaab = auccpm
     C                   eval      stapab = austat
     C                   eval      msgeab = 'SNS8148'
     C                   exsr      Errore
     C                   endif
     C                   endif
     *
     C                   exsr      cabfic
     C                   update    rorcri
     *
     C                   enddo
     *
     C                   endsr
     *
     *****************************************************************
     *    Sped_ctl   : Controlli DDT emesso da CSIS
     *****************************************************************
     C     Sped_ctl      begsr
     *
     C     Kfile         chain     Spvte51l
     C                   if        not %found
     C                   eval      $errore = 'E'
     C                   clear                   zbferr
     C                   eval      ubusab = S1ubus
     C                   eval      tdocab = S1tdoc
     C                   eval      pdocab = S1pdoc
     C                   eval      ndocab = S1ndoc
     C                   eval      msgeab = 'SNS8154'
     C                   exsr      Errore
     C                   leavesr
     C                   endif
     *
     *
     C                   eval      $trovabovte = 'N'
     *
     C     Kfile         setll     Bovte02l
     C                   do        *hival
     C     Kfile         reade     Bovte02l
     C                   if        %EOF
     C                   if        $trovabovte = 'N'
     C                   eval      $errore = 'E'
     C                   clear                   zbferr
     C                   eval      ubusab = S1ubus
     C                   eval      tdocab = S1tdoc
     C                   eval      pdocab = S1pdoc
     C                   eval      ndocab = S1ndoc
     C                   eval      msgeab = 'SNS8150'
     C                   exsr      Errore
     C                   leavesr
     C                   endif
     C                   leave
     C                   endif
     *
     C                   eval      $trovabovte = 'Y'
     C                   eval      $trovabovrg = 'N'
     *
     C     Kbovte        setll     Bovrg51l
     C                   do        *hival
     C     Kbovte        reade     Bovrg51l
     C                   if        %EOF
     C                   if        $trovabovrg = 'N'
     C                   eval      $errore = 'E'
     C                   clear                   zbferr
     C                   eval      ubusab = S1ubus
     C                   eval      tdocab = S1tdoc
     C                   eval      pdocab = S1pdoc
     C                   eval      ndocab = S1ndoc
     C                   eval      msgeab = 'SNS3134'
     C                   exsr      Errore
     C                   leavesr
     C                   endif
     C                   leave
     C                   endif
     *
     C                   eval      $trovabovrg = 'Y'
     c                   clear                   $errore
     *
     *
     C     Kovdrgbox     chain     ovdrg51l
     C                   if        not %found
     C                   eval      $errore = 'E'
     C                   clear                   zbferr
     C                   eval      uborab = brubor
     C                   eval      tdorab = brtdor
     C                   eval      pdorab = brpdor
     C                   eval      ndorab = brndor
     C                   eval      ubptab = S1ubpt
     C                   eval      protab = S1PROT
     C                   eval      nrorab = brnror
     C                   eval      msgeab = 'SNS8153'
     C                   exsr      Errore
     C                   leavesr
     C                   iter
     C                   endif
     *
     C                   eval      rigaord = brnror
     c                   clear                   $numcpa
     *
     *
     C     Kor28box      chain(N)  orcri28k
     C                   if        not %found
     C***                eval      $ubor  = brubor
     C***                eval      $tdor  = brtdor
     C***                eval      $pdor  = brpdor
     C***                eval      $ndor  = brndor
     C***                eval      $nror  = brnror
     C***                eval      $iter       = 'N'
     C***                exsr      Test_OADRG
     C**                 if        $iter = 'Y'
     C***                eval      $errore = 'S'
     C***                leavesr
     C***                endif
     C***                eval      $errore = 'S'
     C***                clear                   zbferr
     C**                 eval      uborab = brubor
     C**                 eval      tdorab = brtdor
     C***                eval      pdorab = brpdor
     C***                eval      ndorab = brndor
     C**+                eval      ubptab = S1ubpt
     C**                 eval      protab = S1PROT
     C**                 eval      nrorab = brnror
     C**                 eval      msgeab = 'SNS8145'
     C**                 exsr      Errore
     C**                 leavesr
     C                   iter
     C                   endif
     *
     C     aumags        chain     magan02k
     C                   if        a003bz <> ##cudb and
     c                             (s1stol<>'ASO' or S1STNW<>'ANN')              berizzi 21102013
     C                   eval      $iter = 'Y'
     C                   eval      $errore = 'Y'
     C                   leavesr
     C                   endif

BET3 * Verifico che non ci siano giacenze sui magazzini SNS
 "   C                   eval      ab1cstg= aucstg
 "   C                   eval      ab1cudb= aucudb
 "   C                   eval      ab1ccli= auccli
 "   C                   eval      ab1nord= aunord
 "   C                   eval      ab1nrig= aunrig
 "   C                   eval      ab1srig= ausrig
 "   C     kmpfan20      Setll     mpfan20K
 "   C                   Do        *hival
 "   C     kmpfan20      Reade     mpfan20K
 "   C                   If        %EOF
 "   C                   Leave
 "   C                   EndIf
 "   C                   If        ab1nuca <> 0 OR ab1qtga = *Zeros
 "   C                   Iter
 "   C                   EndIf
 "   C                   eval      $errore = 'E'
 "   C                   clear                   zbferr
 "   C                   eval      uborab = brubor
 "   C                   eval      tdorab = brtdor
 "   C                   eval      pdorab = brpdor
 "   C                   eval      ndorab = brndor
 "   C                   eval      ubptab = S1ubpt
 "   C                   eval      protab = S1PROT
 "   C                   eval      nrorab = brnror
 "   C                   eval      cstgab = aucstg
 "   C                   eval      cudbab = aucudb
 "   C                   eval      ccliab = auccli
 "   C                   eval      nordab = aunord
 "   C                   eval      nrigab = aunrig
 "   C                   eval      srigab = ausrig
 "   C                   eval      noprab = e6nopr
 "   C                   eval      roprab = e6ropr
 "   C                   eval      srprab = e6srpr
 "   C                   eval      ccpaab = $ccpa
 "   C                   eval      statab = austat
 "   C                   eval      msgeab = 'SN10573'
 "   C                   exsr      Errore
     c                   Leavesr
BET3 C                   EndDo

     *
     *--Se la spedizione vendita CSIS e' stata fatturata, allora
     *--aggiorno SNS con FA
     c                   If        austat = 'SP'
     c                             and tsubfa <> *blanks
     c                   Eval      S1ubus = TSUBFA
     c                   Eval      S1tdoc = TSTDFA
     c                   Eval      S1pdoc = TSPDFA
     c                   Eval      S1ndoc = TSNDFA
     c                   Eval      S1stnw = 'FAT'
     C                   exsr      Fatt_CTL
     C                   IF        $errore = *blanks and aucblk = '   '
     C                   exsr      Fatt_AGG
     C                   endif
     c                   Leavesr
     C                   endif
     *--Controllo status ORCRI solo se Udb <> da Luxso
     c                   eval      $austat=*blank
     c                   if        austat='SL' or austat='RM'
     c                   eval      $austat='Y'
     c                   endif

     C                   if        aucblk = '   ' and $austat<>'Y'
     c                             and ##cudb <> 'LUX'
     C                   eval      $errore = 'E'
     C                   clear                   zbferr
     C                   eval      uborab = brubor
     C                   eval      tdorab = brtdor
     C                   eval      pdorab = brpdor
     C                   eval      ndorab = brndor
     C                   eval      ubptab = S1ubpt
     C                   eval      protab = S1PROT
     C                   eval      nrorab = brnror
     C                   eval      cstgab = aucstg
     C                   eval      cudbab = aucudb
     C                   eval      ccliab = auccli
     C                   eval      nordab = aunord
     C                   eval      nrigab = aunrig
     C                   eval      srigab = ausrig
     C                   eval      ccpaab = $ccpa
     C                   eval      statab = austat
     C                   eval      msgeab = 'SNS2725'
     C                   exsr      Errore
     C                   leavesr
     C                   endif
     *
     C                   if        aucblk = '   ' and austat <> 'RM'
     c                             and ##cudb = 'LUX'
     C                   eval      $errore = 'E'
     C                   clear                   zbferr
     C                   eval      uborab = brubor
     C                   eval      tdorab = brtdor
     C                   eval      pdorab = brpdor
     C                   eval      ndorab = brndor
     C                   eval      ubptab = S1ubpt
     C                   eval      protab = S1PROT
     C                   eval      nrorab = brnror
     C                   eval      cstgab = aucstg
     C                   eval      cudbab = aucudb
     C                   eval      ccliab = auccli
     C                   eval      nordab = aunord
     C                   eval      nrigab = aunrig
     C                   eval      srigab = ausrig
     C                   eval      ccpaab = $ccpa
     C                   eval      statab = austat
     C                   eval      msgeab = 'SNS2725'
     C                   exsr      Errore
     C                   leavesr
     C                   endif
     *
     C                   enddo
     C                   enddo
     *
     C                   endsr
     *
     *****************************************************************
     *    Sped_agg   : Aggiornamenti DDT emessi da CSIS
     *****************************************************************
     C     Sped_agg      begsr
     *
     C     Kfile         chain     Spvte51l
     *
     C     Kfile         setll     Bovte02l
     C                   do        *hival
     C     Kfile         reade     Bovte02l
     C                   if        %EOF
     C                   leave
     C                   endif
     *
     C     Kbovte        setll     Bovrg51l
     C                   do        *hival
     C     Kbovte        reade     Bovrg51l
     C                   if        %EOF
     C                   leave
     C                   endif
     *
     C     Kovdrgbox     chain     ovdrg51l
     *
     C                   eval      rigaord = brnror
     c                   clear                   $numcpa
     *
     *--Non leggo i cambi cliente
     C     Kor28box      chain     orcri28k
     C                   if        not %found(orcri28k)
     C                   iter
     C                   endif
     *
     *--Aggiorno ORCRI
     C                   eval      audubo = tsubsp
     C                   eval      audtdo = tstdsp
     C                   eval      audpdo = tspdsp
     C                   eval      audndo = tsndsp
     C                   eval      audtbo = tsdssp
     C                   eval      audtsp = tsdssp
     C                   eval      audtst = $$dat8
     *
     C                   if        aucblk = *blanks
     C                   eval      austat = 'SP'
     C     austat        chain     evean01k
     C                   if        %found
     C                   eval      austan = cgstan
     C                   endif
     C                   endif
     *
     C                   exsr      Aggio_qtaorcri
     C                   exsr      cabfic
     C                   update    rorcri
     *
     C                   enddo
     C                   enddo
     *
     C                   endsr
     *
     *****************************************************************
     *    Fatt_ctl    : Controlli  fatturazione da CSIS a SNS
     *****************************************************************
     C     Fatt_ctl      begsr
     *
     C                   eval      $trovaspvte = 'N'
     *
     C     Kfile         setll     Spvte19l
     C                   do        *hival
     C     Kfile         reade     Spvte19l
     C                   if        %EOF
     C                   if        $trovaspvte = 'N'
     C                   eval      $errore = 'E'
     C                   clear                   zbferr
     C                   eval      ubusab = S1ubus
     C                   eval      tdocab = S1tdoc
     C                   eval      pdocab = S1pdoc
     C                   eval      ndocab = S1ndoc
     C                   eval      msgeab = 'SNS8154'
     C                   exsr      Errore
     C                   leavesr
     C                   endif
     C                   leave
     C                   endif
     *
     C                   eval      $trovaspvte = 'Y'
     C                   eval      $trovabovte = 'N'
     *
     C     KbovteSPE     setll     Bovte02l
     C                   do        *hival
     C     KbovteSPE     reade     Bovte02l
     C                   if        %EOF
     C                   if        $trovabovte = 'N'
     C                   eval      $errore = 'E'
     C                   clear                   zbferr
     C                   eval      ubusab = S1ubus
     C                   eval      tdocab = S1tdoc
     C                   eval      pdocab = S1pdoc
     C                   eval      ndocab = S1ndoc
     C                   eval      msgeab = 'SNS8150'
     C                   exsr      Errore
     C                   leavesr
     C                   endif
     C                   leave
     C                   endif
     *
     C                   eval      $trovabovte = 'Y'
     C                   eval      $trovabovrg = 'N'
     *
     C     Kbovte        setll     Bovrg51l
     C                   do        *hival
     C     Kbovte        reade     Bovrg51l
     C                   if        %EOF
     C                   if        $trovabovrg = 'N'
     C                   eval      $errore = 'E'
     C                   clear                   zbferr
     C                   eval      ubusab = btubbx
     C                   eval      tdocab = bttdbx
     C                   eval      pdocab = btpdbx
     C                   eval      ndocab = btndbx
     C                   eval      msgeab = 'SNS3134'
     C                   exsr      Errore
     C                   leavesr
     C                   endif
     C                   leave
     C                   endif
     *
     C                   eval      $trovabovrg = 'Y'
     *
     C     Kovdrgbox     chain     ovdrg51l
     C                   if        not %found
     C                   eval      $errore = 'E'
     C                   clear                   zbferr
     C                   eval      uborab = brubor
     C                   eval      tdorab = brtdor
     C                   eval      pdorab = brpdor
     C                   eval      ndorab = brndor
     C                   eval      ubptab = S1ubpt
     C                   eval      protab = S1PROT
     C                   eval      nrorab = brnror
     C                   eval      msgeab = 'SNS8153'
     C                   exsr      Errore
     C                   endif
     *
     C                   eval      rigaord = brnror
     c                   clear                   $numcpa
      *
     C     Kor28box      chain     orcri28k
     C                   if        not %found
     C**                 eval      $ubor  = brubor
     C***                eval      $tdor  = brtdor
     C***                eval      $pdor  = brpdor
     C***                eval      $ndor  = brndor
     C***                eval      $nror  = brnror
     C**                 eval      $iter       = 'N'
     C**                 exsr      Test_OADRG
     C**                 if        $iter = 'Y'
     C**                 eval      $errore = 'S'
     C**                 leavesr
     C**                 endif
     C***                eval      $errore = 'S'
     C***                clear                   zbferr
     C***                eval      uborab = brubor
     C***                eval      tdorab = brtdor
     C**                 eval      pdorab = brpdor
     C***                eval      ndorab = brndor
     C**                 eval      ubptab = S1ubpt
     C**                 eval      protab = S1prot
     C**                 eval      nrorab = brnror
     C**                 eval      msgeab = 'SNS8145'
     C**                 exsr      Errore
     C**                 leavesr
     C                   iter
     C                   endif
     *
     C     aumags        chain     magan02k
     C                   if        a003bz <> ##cudb and
     c                             (s1stol<>'ASO' or S1STNW<>'ANN')              berizzi 21102013
     C                   eval      $iter = 'Y'
     C                   eval      $errore = 'Y'
     C                   leavesr
     C                   endif
     *
     C                   if        aucblk <> *blanks
     C                   eval      $errore = 'S'
     C                   clear                   zbferr
     C                   eval      uborab = brubor
     C                   eval      tdorab = brtdor
     C                   eval      pdorab = brpdor
     C                   eval      ndorab = brndor
     C                   eval      ubptab = S1ubpt
     C                   eval      protab = S1PROT
     C                   eval      nrorab = brnror
     C                   eval      cstgab = aucstg
     C                   eval      cudbab = aucudb
     C                   eval      ccliab = auccli
     C                   eval      nordab = aunord
     C                   eval      nrigab = aunrig
     C                   eval      srigab = ausrig
     C                   eval      noprab = e6nopr
     C                   eval      roprab = e6ropr
     C                   eval      srprab = e6srpr
     C                   eval      ccpaab = $ccpa
     C                   eval      statab = austat
     C                   eval      msgeab = 'SNS9088'
     C                   exsr      Errore
     C                   endif
     *
     C                   if        aucblk = *blanks and
     C                             austat <> 'SP' and austat <> 'TR'
     C                   eval      $errore = 'E'
     C                   clear                   zbferr
     C                   eval      uborab = brubor
     C                   eval      tdorab = brtdor
     C                   eval      pdorab = brpdor
     C                   eval      ndorab = brndor
     C                   eval      ubptab = S1ubpt
     C                   eval      protab = S1PROT
     C                   eval      nrorab = brnror
     C                   eval      cstgab = aucstg
     C                   eval      cudbab = aucudb
     C                   eval      ccliab = auccli
     C                   eval      nordab = aunord
     C                   eval      nrigab = aunrig
     C                   eval      srigab = ausrig
     C                   eval      noprab = e6nopr
     C                   eval      roprab = e6ropr
     C                   eval      srprab = e6srpr
     C                   eval      ccpaab = $ccpa
     C                   eval      statab = austat
     C                   eval      msgeab = 'SNS2725'
     C                   exsr      Errore
     C                   endif
BET3 * Verifico che non ci siano giacenze sui magazzini SNS
 "   C                   eval      ab1cstg= aucstg
 "   C                   eval      ab1cudb= aucudb
 "   C                   eval      ab1ccli= auccli
 "   C                   eval      ab1nord= aunord
 "   C                   eval      ab1nrig= aunrig
 "   C                   eval      ab1srig= ausrig
 "   C     kmpfan20      Setll     mpfan20K
 "   C                   Do        *hival
 "   C     kmpfan20      Reade     mpfan20K
 "   C                   If        %EOF
 "   C                   Leave
 "   C                   EndIf
 "   C                   If        ab1nuca <> 0 OR ab1qtga = *Zeros
 "   C                   Iter
 "   C                   EndIf
 "   C                   eval      $errore = 'E'
 "   C                   clear                   zbferr
 "   C                   eval      uborab = brubor
 "   C                   eval      tdorab = brtdor
 "   C                   eval      pdorab = brpdor
 "   C                   eval      ndorab = brndor
 "   C                   eval      ubptab = S1ubpt
 "   C                   eval      protab = S1PROT
 "   C                   eval      nrorab = brnror
 "   C                   eval      cstgab = aucstg
 "   C                   eval      cudbab = aucudb
 "   C                   eval      ccliab = auccli
 "   C                   eval      nordab = aunord
 "   C                   eval      nrigab = aunrig
 "   C                   eval      srigab = ausrig
 "   C                   eval      noprab = e6nopr
 "   C                   eval      roprab = e6ropr
 "   C                   eval      srprab = e6srpr
 "   C                   eval      ccpaab = $ccpa
 "   C                   eval      statab = austat
 "   C                   eval      msgeab = 'SN10573'
 "   C                   exsr      Errore
BET3 C                   EndDo
     *
     C                   enddo
     C                   enddo
     C                   enddo
     *
     C                   endsr
     *
     *****************************************************************
     *    Fatt_agg    : Aggiornamenti fatturazione da CSIS a SNS
     *****************************************************************
     C     Fatt_agg      begsr
     *
     C     Kfile         setll     Spvte19l
     C                   do        *hival
     C     Kfile         reade     Spvte19l
     C                   if        %EOF
     C                   leave
     C                   endif
     *
     C     KbovteSPE     setll     Bovte02l
     C                   do        *hival
     C     KbovteSPE     reade     Bovte02l
     C                   if        %EOF
     C                   leave
     C                   endif
     *
     C     Kbovte        setll     Bovrg51l
     C                   do        *hival
     C     Kbovte        reade     Bovrg51l
     C                   if        %EOF
     C                   leave
     C                   endif
     *
     C     Kovdrgbox     chain     ovdrg51l
     *
     C                   eval      rigaord = brnror
     c                   clear                   $numcpa
     *
     C     Kor28box      chain     orcri28k
     *
     c                   If        %found(orcri28k)
     *
     C                   If        aucblk <> *blanks
     C                   iter
     C                   Endif
     *
     C                   eval      aufubo = tsubfa
     C                   eval      auftdo = tstdfa
     C                   eval      aufpdo = tspdfa
     C                   eval      aufndo = tsndfa
     C                   eval      audtfa = tsdssp
     C                   eval      audtft = tsdssp
     C                   eval      audtst = $$dat8
     C                   eval      austat = 'FA'
     C     austat        chain     evean01k
     C                   eval      austan = cgstan
     C                   exsr      cabfic
     C                   update    rorcri
     c                   Endif
     *
     C                   enddo
     C                   enddo
     C                   enddo
     *
     C                   endsr
     *
     *****************************************************************
     *    Annullamento: ripristina gli archivi SNS in base allo
     *    annullamento effettuato
     *****************************************************************
     C     Annullamento  begsr
     *
     C                   select
     *
     * Annullamento proposta
     C                   when      S1stol = 'LIS'
     C                   Exsr      Annullo_prop
     *
     * Annullamento SPEDIZIONE
     C                   when      S1stol = 'ISP'
     C                   Exsr      Annullo_DDT
     *
     C                   when      S1stol = 'BOX'
     C                   exsr      Annullo_box
     *
     * Annullamento fatture
     C                   when      S1stol = 'FAT'
     C     Kfile         setll     orcri0ak
     C                   do        *hival
     C     Kfile         reade     orcri0ak
     C                   if        %EOF
     C                   leave
     C                   endif
     C                   eval      $controllo = *ON
     C                   if        austat = 'FA'
     C                   eval      aufubo = *blanks
     C                   eval      auftdo = *blanks
     C                   eval      aufpdo = *blanks
     C                   eval      aufndo = *blanks
     C                   eval      audtfa = *zeros
     C                   eval      audtst = $$dat8
     C     aumags        chain     magan02k
     C                   if        a003bz = auudcf
     C                   eval      austat = 'SP'
     C                   else
     C                   eval      austat = 'TR'
     C                   endif
     C     austat        chain     evean01k
     C                   if        %found
     C                   eval      austan = cgstan
     C                   endif
     C                   exsr      cabfic
     C                   update    orcri0a
     C                   endif
BET  C                   ExSr      RiprAvprdMag
     *
     C                   enddo
     *
     C                   when      S1stol = 'SPE'
     C     Kfile         setll     orcri0dk
     C                   do        *hival
     C     Kfile         reade     orcri0dk
     C                   if        %EOF
     C                   leave
     C                   endif
     *
     C                   clear                   aububo
     C                   clear                   aubtdo
     C                   clear                   aubpdo
     C                   clear                   aubndo
     C                   exsr      cabfic
     C                   update    orcri0d
     C                   enddo
     C                   Exsr      Annullo_DDT
     C                   Exsr      Annullo_Prop
BET  *
 "   * Riporto riga a Premagazzino
 "   C                   when      S1stol = 'MAG'
 "   C                   ExSr      RiportaPremag
BET  *
     *
     C                   endsl
     *
     C                   endsr
     *
     *
     *****************************************************************
     *    Calcolo_forn: Calcolo il prossimo fornitore su ORPRA
     *                   per navettamenti
     *****************************************************************
     C     Calcolo_forn  begsr
     *
     * Verifico UdB provenienza e destinazione
     C     e6masm        chain     magan02k
     C                   eval      $udbpr = a003bz
     C     e6maam        chain     magan02k
     C                   eval      $udbde = a003bz
     C     Kmcocc        chain     mcocc01l
     C                   eval      e6cudf = $udbde
     C                   eval      e6cfor = bhcfor
     *
     C                   endsr
     *
     *****************************************************************
     *    Verifica_prea : Richiamo modulo che verifica il preannullamento
     *    mi scrive gli archivi ORCPA o ORCPP e controlla le quantita
     *****************************************************************
     C     Verifica_prea begsr
     *
     * Se capo accoppiato scrivo solo una volta l'ORCPA
     c                   if        e6ccpa <> e6ccpp
     c                             and $cont > 1
     c                   leavesr
     c                   endif
     *
     C                   clear                   zbf004
     C                   eval      cstgfy = e6cstg
     C                   eval      statfy = S1stnw
     C                   eval      cudbfy = e6cudb
     C                   eval      cclify = e6ccli
     C                   eval      nordfy = e6nord
     C                   eval      nrigfy = e6nrig
     C                   eval      srigfy = e6srig
     C                   eval      noprfy = e6nopr
     C                   eval      roprfy = e6ropr
     C                   eval      srprfy = e6srpr
     C                   eval      ccpafy = e6ccpa
     C                   eval      cvarfy = $cvar
     C                   if        S1ubmg <> *blanks
     C     S1ubmg        chain     udban03k
     C     Kmagcsis      chain     magan08k
     C                   if        %found
     C                   eval      cmagfy = bzcmag
     C                   else
     c                   eval      apcudb = ##CUDB
     C     Kmagcsis      chain     magan08k
     C                   if        %found
     C                   eval      cmagfy = bzcmag
     C                   else
     C                   eval      $errore = 'E'
     C                   clear                   zbferr
     C                   eval      uborab = S1ubor
     C                   eval      tdorab = S1tdor
     C                   eval      pdorab = S1pdor
     C                   eval      ndorab = S1ndor
     C                   eval      ubptab = S1ubpt
     C                   eval      protab = S1prot
     C                   eval      nrorab = S1nror
     C                   eval      magaab = S1maga
     C                   eval      msgeab = 'SNS6073'
     C                   exsr      Errore
     C                   endif
     C                   endif
     C                   endif
     C                   eval      qualfy = $qual
     C                   eval      qtatfy = S1toqn
     C                   eval      qfy01  = S1qn01
     C                   eval      qfy02  = S1qn02
     C                   eval      qfy03  = S1qn03
     C                   eval      qfy04  = S1qn04
     C                   eval      qfy05  = S1qn05
     C                   eval      qfy06  = S1qn06
     C                   eval      qfy07  = S1qn07
     C                   eval      qfy08  = S1qn08
     C                   eval      qfy09  = S1qn09
     C                   eval      qfy10  = S1qn10
     C                   eval      qfy11  = S1qn11
     C                   eval      qfy12  = S1qn12
     C                   eval      qfy13  = S1qn13
     C                   eval      qfy14  = S1qn14
     C                   eval      qfy15  = S1qn15
     C                   eval      qfy16  = S1qn16
     C                   eval      anubfy = S1anub
     C                   eval      anaafy = S1anaa
     C                   eval      anprfy = S1anpr
     c                   if        qtatfy = *zeros
     c                   eval      qtatfy = s1qn01 + s1qn02 + s1qn03 + s1qn04 +
     c                                      s1qn05 + s1qn06 + s1qn07 + s1qn08 +
     c                                      s1qn09 + s1qn10 + s1qn11 + s1qn12 +
     c                                      s1qn13 + s1qn14 + s1qn15 + s1qn16
     c                   endif
     C                   eval      kpjbu  = zbf004
     C                   call      'BF004'
     C                   parm                    kpjba
     C                   eval      zbf004 = kpjbu
     *
     C                   if        critfy <> *blanks
     C                   eval      $errore = 'E'
     C                   clear                   zbferr
     C                   eval      uborab = S1ubor
     C                   eval      tdorab = S1tdor
     C                   eval      pdorab = S1pdor
     C                   eval      ndorab = S1ndor
     C                   eval      ubptab = S1ubpt
     C                   eval      protab = S1prot
     C                   eval      nrorab = S1nror
     C                   eval      ccpaab = $ccpa
     C***                eval      msgeab = 'SNS8273'
     C                   eval      msgeab = msgefy
     C                   exsr      Errore
     C                   endif
     *
     C                   endsr
     *****************************************************************
     *    Allineo_qta: E' stato fatto un preannullamento, quindi
     *                 devo modificare le quantita
     *****************************************************************
     C     Allineo_qta   begsr
     *
     C                   eval      e6q01= e6q01 - afy01
     C                   eval      e6q02= e6q02 - afy02
     C                   eval      e6q03= e6q03 - afy03
     C                   eval      e6q04= e6q04 - afy04
     C                   eval      e6q05= e6q05 - afy05
     C                   eval      e6q06= e6q06 - afy06
     C                   eval      e6q07= e6q07 - afy07
     C                   eval      e6q08= e6q08 - afy08
     C                   eval      e6q09= e6q09 - afy09
     C                   eval      e6q10= e6q10 - afy10
     C                   eval      e6q11= e6q11 - afy11
     C                   eval      e6q12= e6q12 - afy12
     C                   eval      e6q13= e6q13 - afy13
     C                   eval      e6q14= e6q14 - afy14
     C                   eval      e6q15= e6q15 - afy15
     C                   eval      e6q16= e6q16 - afy16
     C                   eval      e6qlan = e6qlan - qtaafy
     *
     * Se la quantita risulta a zero, annullo la riga
     C                   if        e6qlan = *zeros
     C                   eval      e6stat = statb5
     C     e6stat        chain     evean01k
     C                   eval      e6stan = cgstan
     C                   endif
     C                   exsr      cabfic
     C                   update    rorpra
     *
     C     Kscacm        chain     scacm01k
     C                   if        %found
     C     1             do        16            $X
     C                   if        wss($X) <> *zeros
     C                             and e6q($X) < wss($X)
     C                   eval      wss($X) = e6q($X)
     C                   endif
     C                   enddo
     C                   xfoot     wss           wsncps
     C                   if        wsncps = *zeros
     C                   eval      a001ws = 'A'
     C                   endif
     C                   exsr      cabfic
     C                   update    rscacm
     C                   endif
     *
     C                   endsr
     *
     *****************************************************************
     *    Allineo_orcri: E' stato fatto un preannullamento, quindi
     *                   devo modificare le quantita su ORCRI
     *****************************************************************
     C     Allineo_orcri begsr
     *
     C                   eval      audtan = $$dat8
     C                   eval      aucvar = $cvar
     C                   eval      auflpa = 'N'
     *
     * Se la causale di variazione ammette l'aggiornamento delle qta
     C                   if        flqab5 = 'Y'
     C                   eval      aua01= aua01 - afy01
     C                   eval      aua02= aua02 - afy02
     C                   eval      aua03= aua03 - afy03
     C                   eval      aua04= aua04 - afy04
     C                   eval      aua05= aua05 - afy05
     C                   eval      aua06= aua06 - afy06
     C                   eval      aua07= aua07 - afy07
     C                   eval      aua08= aua08 - afy08
     C                   eval      aua09= aua09 - afy09
     C                   eval      aua10= aua10 - afy10
     C                   eval      aua11= aua11 - afy11
     C                   eval      aua12= aua12 - afy12
     C                   eval      aua13= aua13 - afy13
     C                   eval      aua14= aua14 - afy14
     C                   eval      aua15= aua15 - afy15
     C                   eval      aua16= aua16 - afy16
     C                   eval      aucqta = aucqta - qtaafy
     C                   endif
     *
     * Se la quantita risulta a zero, annullo la riga
     C                   if        aucqta = *zeros
     C                   eval      austat = statb5
     C     austat        chain     evean01k
     C                   eval      austan = cgstan
     C                   eval      audtst = $$dat8
     C                   eval      auflbx = 'N'
     C                   endif
     *
     C                   exsr      cabfic
     C                   update    rorcri
     *
     C                   endsr
     *
     *****************************************************************
     *    Calcolo_MGN: Calcolo mediante modulo se magazzino di navett.
     *
     *****************************************************************
     C     Calcolo_MGN   begsr
     *
     *
     C                   clear                   zmp708
     C                   eval      cudbex = e6cudb
     C                   eval      ccliex = e6ccli
     C                   eval      cstgex = e6cstg
     C                   eval      nordex = e6nord
     C                   eval      nrigex = e6nrig
     C                   eval      srigex = e6srig
     C                   eval      noprex = e6nopr
     C                   eval      nrprex = e6ropr
     C                   eval      srprex = e6srpr
     C                   eval      ccpaex = e6ccpa
     C                   eval      kpjbu =zmp708
     C                   call      'MP708'
     C                   parm                    kpjba
     C                   eval      zmp708 = kpjbu
     *
     C                   endsr
     *
     *****************************************************************
     *    Aggio_qtaorcri : Aggiorno le quantita su ORCRI
     *****************************************************************
     C     Aggio_qtaorcribegsr
     *
     C                   select
     *
     * Aggiorno dati versamento riga (quantita e data)
     C                   when      S1stnw = 'MAG'
 "   * Uso taglie CSIS trascodificate per SNS
 "   C                   eval      aum01= aua01
 "   C                   eval      aum02= aua02
 "   C                   eval      aum03= aua03
 "   C                   eval      aum04= aua04
 "   C                   eval      aum05= aua05
 "   C                   eval      aum06= aua06
 "   C                   eval      aum07= aua07
 "   C                   eval      aum08= aua08
 "   C                   eval      aum09= aua09
 "   C                   eval      aum10= aua10
 "   C                   eval      aum11= aua11
 "   C                   eval      aum12= aua12
 "   C                   eval      aum13= aua13
 "   C                   eval      aum14= aua14
 "   C                   eval      aum15= aua15
 "   C                   eval      aum16= aua16
 "   C                   eval      aucqtm = aucqta
     *
 "   C                   eval      audtma = $$dat8
     *
     C                   when      S1stnw = 'ISP'
     C                             or S1stnw = 'SPE'
     C                   eval      aus01= aum01
     C                   eval      aus02= aum02
     C                   eval      aus03= aum03
     C                   eval      aus04= aum04
     C                   eval      aus05= aum05
     C                   eval      aus06= aum06
     C                   eval      aus07= aum07
     C                   eval      aus08= aum08
     C                   eval      aus09= aum09
     C                   eval      aus10= aum10
     C                   eval      aus11= aum11
     C                   eval      aus12= aum12
     C                   eval      aus13= aum13
     C                   eval      aus14= aum14
     C                   eval      aus15= aum15
     C                   eval      aus16= aum16
     C                   eval      aucqts = aucqtm
     *
     C                   when      S1stnw = 'ANN'
     C                             and S1stol = 'ISP'
     C                   eval      aus01= 0
     C                   eval      aus02= 0
     C                   eval      aus03= 0
     C                   eval      aus04= 0
     C                   eval      aus05= 0
     C                   eval      aus06= 0
     C                   eval      aus07= 0
     C                   eval      aus08= 0
     C                   eval      aus09= 0
     C                   eval      aus10= 0
     C                   eval      aus11= 0
     C                   eval      aus12= 0
     C                   eval      aus13= 0
     C                   eval      aus14= 0
     C                   eval      aus15= 0
     C                   eval      aus16= 0
     C                   eval      aucqts = 0
     *
     C                   endsl
     *
     C                   endsr
     *
     *****************************************************************
     *    Testata_box   : Creo testata box SNS da Bovte
     *****************************************************************
     C     Testata_box   begsr
     *
     C                   if        btplbx < btpnbx
     C                   eval      $errore = 'E'
     C                   clear                   zbferr
     C                   eval      ubusab = S1ubus
     C                   eval      tdocab = S1tdoc
     C                   eval      pdocab = S1pdoc
     C                   eval      ndocab = S1ndoc
     C                   eval      msgeab = 'SNS4083'
     C                   exsr      Errore
     C                   endif
     *
     C                   clear                   RS14TE
     C     btubbx        chain     udban03k
     C                   if        %found
     C                   eval      goudbb = apcudb
     C                   else
     C                   eval      $errore = 'E'
     C                   clear                   zbferr
     C                   eval      ubusab = S1ubus
     C                   eval      tdocab = S1tdoc
     C                   eval      pdocab = S1pdoc
     C                   eval      ndocab = S1ndoc
     C                   eval      ubusab = btubbx
     C                   eval      msgeab = 'SNS7166'
     C                   exsr      Errore
     C                   endif
     C                   movel     btpdbx        $anno
     C                   move      $anno         goannb
     C                   clear                   $numerobox
     C                   eval      $numerobox = btndbx
     C                   move      $seriebox     gosbox
     C                   move      $numbox       gonumb
     *
     C     KS14TE        chain(N)  S14te01k
     C                   if        %found(S14te01k)
     C                   eval      $errore = 'E'
     C*                  clear                   zbferr
     C*                  eval      ubusab = S1ubus
     C*                  eval      tdocab = S1tdoc
     C*                  eval      pdocab = S1pdoc
     C*                  eval      ndocab = S1ndoc
     C*                  eval      msgeab = 'SNS5099'
     C*                  exsr      Errore
     C                   leavesr
     C                   endif
     * Trascodifico la TPB (Tipo box)
     C                   Movel(P)  'TPB'         $ctab
     C                   Movel(P)  bttpbx        $cele
     C                   Exsr      Trasco_tabe
     C                   if        @tsc2 <> *blanks
     C                   movel     @tsc2         goctpb
     C                   else
     C                   eval      $errore = 'E'
     C                   clear                   zbferr
     C                   eval      ubusab = S1ubus
     C                   eval      tdocab = S1tdoc
     C                   eval      pdocab = S1pdoc
     C                   eval      ndocab = S1ndoc
     C                   eval      ctpcab = bttpbx
     C                   eval      msgeab = 'SNS7162'
     C                   exsr      Errore
     C                   endif
     *
     * Prendo i dati dalla tabella TPB (Tipo box)
     C                   if        goctpb <> *blanks
     C                   Movel(P)  'TPB'         $ctab
     C                   Movel(P)  goctpb        $cele
     C                   Exsr      Ctl_decotab
     C                   If        @DLI3 = '1'
     C                   eval      $errore = 'E'
     C                   clear                   zbferr
     C                   eval      ubusab = S1ubus
     C                   eval      tdocab = S1tdoc
     C                   eval      pdocab = S1pdoc
     C                   eval      ndocab = S1ndoc
     C                   eval      ctpbab = goctpb
     C                   eval      msgeab = 'SNS7163'
     C                   exsr      Errore
     C                   Else
     C                   Movel(P)  @dli2         z@ctpb
     C                   endif
     C                   endif
     *
     C                   eval      gopsld = btplbx
     C                   eval      gopsnt = btpnbx
     C                   eval      gotara = gopsld - gopsnt
     C                   eval      govolu = btvolb
     C                   eval      goalte = alteg5
     C                   eval      golung = lungg5
     C                   eval      goprof = profg5
     C                   eval      goponu = btponu
     *
     C                   endsr
     *
     *****************************************************************
     *    Wrt_SSCCB     : Creo sovraccollo in SNS
     *****************************************************************
     C     Wrt_SSCCB     begsr
     *
     C                   clear                   RSSCCB
     C                   eval      v5udbb = goudbb
     C                   eval      v5annb = goannb
     C                   eval      v5sbox = gosbox
     C                   eval      v5numb = gonumb
     C                   eval      v5sscc = nwsscc
     C                   eval      v5ponu = goponu
     C                   eval      a003v5 = ##cudb
     C                   eval      a004v5 = ##cudb
     C                   eval      a005v5 = ##cudb
     *
     C                   exsr      cabfic
     C                   write     rssccb
     *
     C                   endsr
     *
     *****************************************************************
     *    Righe_box: Creo righe box SNS da Bovrg
     *****************************************************************
     C     Righe_box     begsr
     *
     C     Kovdrgbox     chain     ovdrg51l
     C                   if        not %found
     C                   eval      $errore = 'E'
     C                   clear                   zbferr
     C                   eval      uborab = brubor
     C                   eval      tdorab = brtdor
     C                   eval      pdorab = brpdor
     C                   eval      ndorab = brndor
     C                   eval      ubptab = S1ubpt
     C                   eval      protab = S1PROT
     C                   eval      nrorab = brnror
     C                   eval      msgeab = 'SNS8153'
     C                   exsr      Errore
     C                   endif

     C                   eval      rigaord = brnror
     c                   clear                   $numcpa

     C     Kor28box      chain     orcri28K
     C                   if        not %found
     C                   eval      $ubor  = brubor
     C                   eval      $tdor  = brtdor
     C                   eval      $pdor  = brpdor
     C                   eval      $ndor  = brndor
     C                   eval      $nror  = brnror
     C                   exsr      Test_OADRG
     C                   if        $iter = 'Y'
     C                   leavesr
     C                   endif
     C                   eval      $errore = 'E'
     C                   clear                   zbferr
     C                   eval      uborab = brubor
     C                   eval      tdorab = brtdor
     C                   eval      pdorab = brpdor
     C                   eval      ndorab = brndor
     C                   eval      ubptab = S1ubpt
     C                   eval      protab = S1prot
     C                   eval      nrorab = brnror
     C                   eval      msgeab = 'SNS8145'
     C                   exsr      Errore
     C                   else
     C                   eval      auflbx = 'Y'
     c                   if        govolu <> *zeros
     c                   eval      auctsp = 'BOX'
     c                   else
     c                   eval      auctsp = 'BXV'
     c                   endif
     C                   exsr      cabfic
     C                   update    rorcri
     C                   endif
     *
     *
     C                   clear                   RS14RI
     C     brubbx        chain     udban03k
     C                   if        %found
     C                   eval      gpudbb = apcudb
     C                   else
     C                   eval      $errore = 'E'
     C                   clear                   zbferr
     C                   eval      ubusab = S1ubus
     C                   eval      tdocab = S1tdoc
     C                   eval      pdocab = S1pdoc
     C                   eval      ndocab = S1ndoc
     C                   eval      ubusab = brubbx
     C                   eval      msgeab = 'SNS7166'
     C                   exsr      Errore
     C                   endif
     C                   movel     brpdbx        $anno
     C                   move      $anno         gpannb
     C                   move      $seriebox     gpsbox
     C                   move      $numbox       gpnumb
     *
     C                   eval      gpnrgb = brnrbx
     C                   eval      gpudbo = aucudb
     C                   eval      gpccli = auccli
     C                   eval      gpnord = aunord
     C                   eval      gpnrig = aunrig
     C                   eval      gpsrig = ausrig
     C                   eval      gpcstg = aucstg
     *
     * Trascodifico la TGL (Tipo taglia)
     C                   Movel(P)  'TGL'         $ctab
     C                   Movel(P)  brtptg        $cele
     C                   Exsr      Trasco_tabe
     C                   if        @tsc2 <> *blanks
     C                   movel     @tsc2         gpttag
     C                   else
     C                   eval      $errore = 'E'
     C                   clear                   zbferr
     C                   eval      uborab = brubor
     C                   eval      tdorab = brtdor
     C                   eval      pdorab = brpdor
     C                   eval      ndorab = brndor
     C                   eval      ubptab = S1ubpt
     C                   eval      protab = S1PROT
     C                   eval      nrorab = brnror
     C                   eval      msgeab = 'SNS8184'
     C                   exsr      Errore
     C                   endif
     *
     * Verifico se l'elemento trascodificato dalla tabella TGL esiste in SNS
     C                   if        gpttag <> *blanks
     C                   Movel(P)  'TGL'         $ctab
     C                   Movel(P)  gpttag        $cele
     C                   Exsr      Ctl_decotab
     C                   If        @DLI3 = '1'
     C                   eval      $errore = 'E'
     C                   clear                   zbferr
     C                   eval      uborab = brubor
     C                   eval      tdorab = brtdor
     C                   eval      pdorab = brpdor
     C                   eval      ndorab = brndor
     C                   eval      ubptab = S1ubpt
     C                   eval      protab = S1PROT
     C                   eval      nrorab = brnror
     C                   eval      msgeab = 'SNS5773'
     C                   exsr      Errore
     C                   endif
     C                   endif
     *
     * Trascodifico taglie CSIS per SNS
     C                   eval      $trascotgl = '2'
     C                   eval      $ttag = gpttag
     C                   eval      tgl01  = brqbx1
     C                   eval      tgl02  = brqbx2
     C                   eval      tgl03  = brqbx3
     C                   eval      tgl04  = brqbx4
     C                   eval      tgl05  = brqbx5
     C                   eval      tgl06  = brqbx6
     C                   eval      tgl07  = brqbx7
     C                   eval      tgl08  = brqbx8
     C                   eval      tgl09  = brqbx9
     C                   eval      tgl10  = brqb10
     C                   eval      tgl11  = brqb11
     C                   eval      tgl12  = brqb12
     C                   eval      tgl13  = brqb13
     C                   eval      tgl14  = brqb14
     C                   eval      tgl15  = brqb15
     C                   eval      tgl16  = brqb16
     C                   Exsr      Trasco_tgl
     C                   eval      gpq01 = rln01
     C                   eval      gpq02 = rln02
     C                   eval      gpq03 = rln03
     C                   eval      gpq04 = rln04
     C                   eval      gpq05 = rln05
     C                   eval      gpq06 = rln06
     C                   eval      gpq07 = rln07
     C                   eval      gpq08 = rln08
     C                   eval      gpq09 = rln09
     C                   eval      gpq10 = rln10
     C                   eval      gpq11 = rln11
     C                   eval      gpq12 = rln12
     C                   eval      gpq13 = rln13
     C                   eval      gpq14 = rln14
     C                   eval      gpq15 = rln15
     C                   eval      gpq16 = rln16
     *
     C                   add       gpq01         gpcapi
     C                   add       gpq02         gpcapi
     C                   add       gpq03         gpcapi
     C                   add       gpq04         gpcapi
     C                   add       gpq05         gpcapi
     C                   add       gpq06         gpcapi
     C                   add       gpq07         gpcapi
     C                   add       gpq08         gpcapi
     C                   add       gpq09         gpcapi
     C                   add       gpq10         gpcapi
     C                   add       gpq11         gpcapi
     C                   add       gpq12         gpcapi
     C                   add       gpq13         gpcapi
     C                   add       gpq14         gpcapi
     C                   add       gpq15         gpcapi
     C                   add       gpq16         gpcapi
     *
     * Trascodifico la FAM (Famiglia)
     C                   Movel(P)  'FAM'         $ctab
     C                   Movel(P)  bricpr        $cele
     C                   Exsr      Trasco_tabe
     C                   if        @tsc2 <> *blanks
     C                   movel     @tsc2         gpcfam
     C                   else
     C                   eval      $errore = 'E'
     C                   clear                   zbferr
     C                   eval      uborab = brubor
     C                   eval      tdorab = brtdor
     C                   eval      pdorab = brpdor
     C                   eval      ndorab = brndor
     C                   eval      ubptab = S1ubpt
     C                   eval      protab = S1PROT
     C                   eval      nrorab = brnror
     C                   eval      msgeab = 'SNS8184'
     C                   exsr      Errore
     C                   endif
     *
     * Verifico se l'elemento trascodificato dalla tabella FAM esiste in SNS
     C                   if        gpcfam <> *blanks
     C                   Movel(P)  'FAM'         $ctab
     C                   Movel(P)  gpcfam        $cele
     C                   Exsr      Ctl_decotab
     C                   If        @DLI3 = '1'
     C                   eval      $errore = 'E'
     C                   clear                   zbferr
     C                   eval      uborab = brubor
     C                   eval      tdorab = brtdor
     C                   eval      pdorab = brpdor
     C                   eval      ndorab = brndor
     C                   eval      ubptab = S1ubpt
     C                   eval      protab = S1PROT
     C                   eval      cfamab = gpcfam
     C                   eval      msgeab = 'SNS1022'
     C                   exsr      Errore
     C                   endif
     C                   endif
     * Trascodifico la CPM (Categoria prodotto modello)
     C                   eval      rigaord = brnror
     C                   if        $numcpa <> 0
     C     $numcpa       chain     ctppn01l
     C                   if        %found
     C                   eval      gpccpm = pntpps
     C                   else
     C                   eval      $errore = 'E'
     C                   clear                   zbferr
     C                   eval      uborab = brubor
     C                   eval      tdorab = brtdor
     C                   eval      pdorab = brpdor
     C                   eval      ndorab = brndor
     C                   eval      ubptab = S1ubpt
     C                   eval      protab = S1PROT
     C                   eval      nrorab = brnror
     C                   eval      msgeab = 'SNS7161'
     C                   exsr      Errore
     C                   endif
     C                   else
     C                   eval      gpccpm = auccpm
     C                   endif
     *
     * Verifico se l'elemento trascodificato dalla tabella CPM esiste in SNS
     C                   if        gpccpm <> *blanks
     C                   Movel(P)  'CPM'         $ctab
     C                   Movel(P)  gpccpm        $cele
     C                   Exsr      Ctl_decotab
     C                   If        @DLI3 = '1'
     C                   eval      $errore = 'E'
     C                   clear                   zbferr
     C                   eval      uborab = brubor
     C                   eval      tdorab = brtdor
     C                   eval      pdorab = brpdor
     C                   eval      ndorab = brndor
     C                   eval      ubptab = S1ubpt
     C                   eval      protab = S1PROT
     C                   eval      ccpaab = gpccpm
     C                   eval      msgeab = 'SNS3844'
     C                   exsr      Errore
     C                   endif
     C                   endif
     *
     C                   eval      gpflsl= 'N'
     *
     C                   endsr
     *
     *****************************************************************
     *    Crea_Scatola: Creo scatola bianca
     *****************************************************************
     C     Crea_Scatola  begsr
     *
     C     Kfile         chain     confa01l
     C     Kconfbrg      chain     bovrg51l
     *
     C                   eval      rigaord = brnror
     c                   clear                   $numcpa
     *
     C     Kor28box      chain     orcri28k
     C                   eval      e6nopr = aunord
     C                   eval      e6ropr = aunrig
     C                   eval      e6srpr = ausrig
     C     KorpraSNS     chain     orpra55k
     *
     C                   move      c2ndcf        wsnpac
     C                   eval      wsnpro = c2nrcf
     C     Kscacm02      chain     scacm02k
     C                   if        not %found
     C                   clear                   SCA02
     C                   eval      wscfam = aucfam
     C                   eval      wscudb = aucudb
     C                   eval      wscstg = aucstg
     C                   eval      wsccli = auccli
     C                   eval      wsnord = aunord
     C                   eval      wsnrig = aunrig
     C                   eval      wssrig = ausrig
     C                   eval      wsnopr = e6nopr
     C                   eval      wsropr = e6ropr
     C                   eval      wssrpr = e6srpr
     C                   eval      wsccpp = e6ccpp
     C                   move      c2ndcf        wsnpac
     C                   eval      wsnpro = c2nrcf
     C                   move      c2pdcf        wsastg
     C                   eval      wsnlan = e6nlan
NLANC                   eval      WSNLA4 = e6nlan
     C                   eval      wsngtt = e6ngtt
     C*                  eval      wsctps = xxxxxx
     C*                  eval      wsncps = xxxxxx
     * Trascodifico taglie CSIS per SNS
     C                   eval      $trascotgl = '2'
     C                   eval      $ttag = auttag
     C                   eval      tgl01  = s1qn01
     C                   eval      tgl02  = s1qn02
     C                   eval      tgl03  = s1qn03
     C                   eval      tgl04  = s1qn04
     C                   eval      tgl05  = s1qn05
     C                   eval      tgl06  = s1qn06
     C                   eval      tgl07  = s1qn07
     C                   eval      tgl08  = s1qn08
     C                   eval      tgl09  = s1qn09
     C                   eval      tgl10  = s1qn10
     C                   eval      tgl11  = s1qn11
     C                   eval      tgl12  = s1qn12
     C                   eval      tgl13  = s1qn13
     C                   eval      tgl14  = s1qn14
     C                   eval      tgl15  = s1qn15
     C                   eval      tgl16  = s1qn16
     C                   Exsr      Trasco_tgl
     C                   eval      wss01  = rln01
     C                   eval      wss02  = rln02
     C                   eval      wss03  = rln03
     C                   eval      wss04  = rln04
     C                   eval      wss05  = rln05
     C                   eval      wss06  = rln06
     C                   eval      wss07  = rln07
     C                   eval      wss08  = rln08
     C                   eval      wss09  = rln09
     C                   eval      wss10  = rln10
     C                   eval      wss11  = rln11
     C                   eval      wss12  = rln12
     C                   eval      wss13  = rln13
     C                   eval      wss14  = rln14
     C                   eval      wss15  = rln15
     C                   eval      wss16  = rln16
     C                   eval      wscol3 = aucol3
     C                   eval      wscst3 = aucst3
     C                   eval      a003ws = ##cudb
     C                   eval      a004ws = ##cudb
     C                   eval      a005ws = ##cudb
     C                   exsr      cabfic
     C                   write     Sca02
     C                   endif
     *
     C                   endsr
     *
     *****************************************************************
     *    Ver_triangola: Verifica triangolazione
     *****************************************************************
     C     Ver_triangola begsr
     *
     C                   do
     *
     C                   eval      a003gg = ##cudb
     C                   eval      cstggg = aucstg
     C                   eval      cudbgg = aucudb
     C                   eval      ccligg = auccli
     C                   eval      nordgg = aunord
     C                   eval      nriggg = aunrig
     C                   eval      sriggg = ausrig
     C                   eval      testgg = $testata
     C                   eval      datagg = $$dat8
     C                   eval      udbegg = S1ubus
     C                   eval      rstdgg = S1tdoc
     C                   eval      rpdogg = S1pdoc
     C                   eval      rndogg = S1ndoc
     C                   eval      kpjbu = zbf006
     C**                 call      'BF006'
     C**                 parm                    kpjba
     C                   eval      zbf006 = kpjbu
      *
     C                   enddo
     *
     C                   endsr
     *
     *****************************************************************
     *    Scarico_Mag: scarico magazzino di transito SNS
     *****************************************************************
     C     Scarico_Mag   begsr
     *
     C                   do
     *
     C     ##cudb        chain     udbmf01k
     *
     *se nn trova rek di giacenza sul mag.transito (o = 0 ) mette flag di sospeso sul rek SNS100F
     c                   eval      b1nuca=0
     C     KMPFAN        chain     MPFAN01K
     C                   if        NOT %FOUND
     C                   eval      $ITER = 'Y'
BET  C                   Leave
     C                   endif
     c                   if        b1qtga<=0
     C                   eval      $ITER = 'Y'
BET  C                   Leave
     C                   endif
     *
     C                   clear                   zmp004

BET  C                   eval      NPGMAC = @PGM
     C                   eval      damoac = $$dat8
     C                   eval      udblac = ##cudb
     C                   eval      cmagac = h5cman
     C                   eval      cmmgac = h5cscn
     C                   eval      cstgac = aucstg
     C                   eval      ccpmac = auccpm
BET  C*                  eval      ftriac = 'Y'
BET  C                   eval      ftriac = 'N'
     C                   eval      mudbac = 'Y'
     C                   eval      udboac = ##cudb
BET  C                   If        aucblk = *Blanks
     C                   eval      qt101 = aua01
     C                   eval      qt102 = aua02
     C                   eval      qt103 = aua03
     C                   eval      qt104 = aua04
     C                   eval      qt105 = aua05
     C                   eval      qt106 = aua06
     C                   eval      qt107 = aua07
     C                   eval      qt108 = aua08
     C                   eval      qt109 = aua09
     C                   eval      qt110 = aua10
     C                   eval      qt111 = aua11
     C                   eval      qt112 = aua12
     C                   eval      qt113 = aua13
     C                   eval      qt114 = aua14
     C                   eval      qt115 = aua15
     C                   eval      qt116 = aua16
     C                   eval      qtmoac = aucqta
BET  C                   Else
 "   * uso Trascodifica taglie CSIS per SNS
 "   C                   eval      qt101 = rlns(1)
 "   C                   eval      qt102 = rlns(2)
 "   C                   eval      qt103 = rlns(3)
 "   C                   eval      qt104 = rlns(4)
 "   C                   eval      qt105 = rlns(5)
 "   C                   eval      qt106 = rlns(6)
 "   C                   eval      qt107 = rlns(7)
 "   C                   eval      qt108 = rlns(8)
 "   C                   eval      qt109 = rlns(9)
 "   C                   eval      qt110 = rlns(10)
 "   C                   eval      qt111 = rlns(11)
 "   C                   eval      qt112 = rlns(12)
 "   C                   eval      qt113 = rlns(13)
 "   C                   eval      qt114 = rlns(14)
 "   C                   eval      qt115 = rlns(15)
 "   C                   eval      qt116 = rlns(16)
 "   C                   eval      qtmoac = S1TOQN
BET  C                   EndIf
     C                   eval      cudbac = aucudb
     C                   eval      ccliac = auccli
     C                   eval      nordac = aunord
     C                   eval      nrigac = aunrig
     C                   eval      srigac = ausrig
     C                   eval      cfamac = aucfam
     C                   eval      cdopac = 'I'
     C                   eval      mudbac = 'Y'
     C                   eval      fmomac = 'N'
     C                   eval      fmriac = 'N'
     C                   eval      fincac = 'N'
     C                   eval      flmaac = 'N'
     C                   eval      udmpac = ##cudb
     C                   eval      magpac = h5cman
     C                   eval      udmdac = ##cudb
     C                   eval      magdac = 'WMS'

     C                   call      'MP004'
     C                   parm                    kpjba
     C                   parm                    zmp004
      *
     C                   if        critac <> *blanks
     C                   eval      $errore = 'E'
     C                   clear                   zbferr
     C                   eval      cstgab = aucstg
     C                   eval      cudbab = aucudb
     C                   eval      ccliab = auccli
     C                   eval      nordab = aunord
     C                   eval      nrigab = aunrig
     C                   eval      srigab = ausrig
     C                   eval      ccpaab = auccpm
     C                   eval      msgeab = msgeac
     C                   exsr      Errore
BET  C                   Leave
     C                   endif

     C     Kovtcm        chain     OVTCM01L
     c                   if        %found(OVTCM01L)
     c                   eval      qmdvms = audtma
     c                   eval      qmdtum = %dec (%date() : *iso)
     c                   time                    qmhhum
     c                   eval      qmnmus = ppcute
     c                   eval      qmpgum = ppnpgm
     c                   eval      qmazum = ppnjob
     c                   Update    OVTCM
     c                   endif
     C     Kovtcm        Chain     Totcm01l
     C                   If        %Found (Totcm01l)
     C                   Eval      z_qmdvms = audtma
     C                   Eval      z_qmdtum = %dec (%date() : *iso)
     C                   Time                    z_qmhhum
     C                   Eval      z_qmnmus = ppcute
     C                   Eval      z_qmpgum = ppnpgm
     C                   Eval      z_qmazum = ppnjob
     C                   Update    rTOTCM
     C                   EndIf

BET  * Richiamo modulo per scrittura avanzamenti produttivi
 "   C                   Clear                   zsn680
 "   C                   Eval      $_pgmci2 = 'BF003L'
 "   C                   Eval      $_FICOI2 = 'Y'
 "   C                   Eval      $_GECOI2 = 'N'
 "   C                   Eval      $_CUDBI2 = aucudb
 "   C                   Eval      $_CCLII2 = auccli
 "   C                   Eval      $_CSTGI2 = aucstg
 "   C                   Eval      $_NORDI2 = aunord
 "   C                   Eval      $_NRIGI2 = aunrig
 "   C                   Eval      $_SRIGI2 = ausrig
 "   C                   Eval      $_NOPRI2 = aunord
 "   C                   Eval      $_ROPRI2 = aunrig
 "   C                   Eval      $_SRPRI2 = ausrig
 "   C                   Eval      $_CCPPI2 = e6ccpp
 "
 "   C                   If        aucblk = *Blanks
 "   C                   Eval      $_QTATI2 = aucqta
 "   C                   Eval      $_QTA01  = aua01
 "   C                   Eval      $_QTA02  = aua02
 "   C                   Eval      $_QTA03  = aua03
 "   C                   Eval      $_QTA04  = aua04
 "   C                   Eval      $_QTA05  = aua05
 "   C                   Eval      $_QTA06  = aua06
 "   C                   Eval      $_QTA07  = aua07
 "   C                   Eval      $_QTA08  = aua08
 "   C                   Eval      $_QTA09  = aua09
 "   C                   Eval      $_QTA10  = aua10
 "   C                   Eval      $_QTA11  = aua11
 "   C                   Eval      $_QTA12  = aua12
 "   C                   Eval      $_QTA13  = aua13
 "   C                   Eval      $_QTA14  = aua14
 "   C                   Eval      $_QTA15  = aua15
 "   C                   Eval      $_QTA16  = aua16
 "   C                   Else
 "   * uso Trascodifico taglie CSIS per SNS
 "   C                   eval      $_QTA01  = rlns(1)
 "   C                   eval      $_QTA02  = rlns(2)
 "   C                   eval      $_QTA03  = rlns(3)
 "   C                   eval      $_QTA04  = rlns(4)
 "   C                   eval      $_QTA05  = rlns(5)
 "   C                   eval      $_QTA06  = rlns(6)
 "   C                   eval      $_QTA07  = rlns(7)
 "   C                   eval      $_QTA08  = rlns(8)
 "   C                   eval      $_QTA09  = rlns(9)
 "   C                   eval      $_QTA10  = rlns(10)
 "   C                   eval      $_QTA11  = rlns(11)
 "   C                   eval      $_QTA12  = rlns(12)
 "   C                   eval      $_QTA13  = rlns(13)
 "   C                   eval      $_QTA14  = rlns(14)
 "   C                   eval      $_QTA15  = rlns(15)
 "   C                   eval      $_QTA16  = rlns(16)
 "   C                   eval      $_QTATI2 = S1TOQN
 "   C                   EndIf
 "
 "   *  Scarico tutte le fasi di premagazzino
 "   C                   Eval      $_UDB1I2 = ##cudb
 "   C                   Eval      $_CFP1I2 = h5fpre
 "   C                   Eval      $_TOP1I2 = 'SPM'
 "   C                   Eval      $kpjbu   = kpjbu
 "   C                   Eval      kpjbu    = zsn680
 "
 "   C*                  Call      'SN680'
 "   C*                  Parm                    kpjba
 "
 "   C                   Eval      zsn680   = kpjbu
 "   C                   Eval      kpjbu    = $kpjbu
 "   C                   If        $_criti2 = 'ER'
 "   C*                  eval      $errore = 'E'
 "   C                   clear                   zbferr
 "   C                   eval      cstgab = aucstg
 "   C                   eval      cudbab = aucudb
 "   C                   eval      ccliab = auccli
 "   C                   eval      nordab = aunord
 "   C                   eval      nrigab = aunrig
 "   C                   eval      srigab = ausrig
 "   C                   eval      ccpaab = auccpm
 "   C                   eval      msgeab = $_cmsgi2
 "   C*                  exsr      Errore
 "   C                   EndIf
 "
 "   *  Carico la fase di magazzino
 "   C                   Clear                   $_CritI2
 "   C                   Eval      $_CCPPI2 = e6ccpp
 "   C                   Eval      $_CCP1I2 = auccpm
 "   C                   Eval      $_CPA1I2 = e6ccpa                            **LEL**
 "   C                   Eval      $_UDB1I2 = ##cudb
 "   C                   Eval      $_CFP1I2 = h5fmag
 "   C                   Eval      $_TOP1I2 = 'CAR'
 "   C                   Eval      $kpjbu   = kpjbu
 "   C                   Eval      kpjbu    = zsn680
 "
 "   C*                  Call      'SN680'
 "   C*                  Parm                    kpjba
 "
 "   C                   Eval      zsn680   = kpjbu
 "   C                   Eval      kpjbu    = $kpjbu
 "   C                   If        $_criti2 = 'ER'
 "   C*                  eval      $errore = 'E'
 "   C                   clear                   zbferr
 "   C                   eval      cstgab = aucstg
 "   C                   eval      cudbab = aucudb
 "   C                   eval      ccliab = auccli
 "   C                   eval      nordab = aunord
 "   C                   eval      nrigab = aunrig
 "   C                   eval      srigab = ausrig
 "   C                   eval      ccpaab = auccpm
 "   C                   eval      msgeab = $_cmsgi2
 "   C*                  exsr      Errore
BET  C                   EndIf
      *
      *
     C                   enddo
     *
     C                   endsr
     *
     *****************************************************************
     *    Carico_Magtri: Carico magazzino di triangolazione remoto
     *****************************************************************
     C     Carico_Magtri begsr
     *
     C                   do
     *
     C                   clear                   zmp004

     C                   eval      udblac = udbmgg
     C                   eval      cmagac = cmaggg
     C                   eval      cmmgac = caucgg
     C                   eval      cstgac = e6cstg
     C                   eval      ccpmac = e6ccpa
     C                   eval      ftriac = 'Y'
     C                   eval      mudbac = 'Y'
     C                   eval      udboac = udbmgg
     C                   Select
     C     A006AU        Wheneq    *BLANKS
     C     A005AU        ANDNE     ##CUDB
     C                   MOvel     ##CUDB        UDBOAC
     C                   MOVEL     'Y'           MUDBAC
     C     A005AU        WHEneQ    ##CUDB
     C     A006AU        ANDNE     ##CUDB
     C     A006AU        ANDNE     *BLANKS
     C                   mOVEL     *BLANKS       UDBOAC
     C                   MOVEL     'Y'           MUDBAC
     C                   OTHER
     C                   MOVEL     *BLANKS       UDBOAC
     C                   MOVEL     'N'           MUDBAC
     C                   EndSl
     C                   eval      qt101 = e6q01
     C                   eval      qt102 = e6q02
     C                   eval      qt103 = e6q03
     C                   eval      qt104 = e6q04
     C                   eval      qt105 = e6q05
     C                   eval      qt106 = e6q06
     C                   eval      qt107 = e6q07
     C                   eval      qt108 = e6q08
     C                   eval      qt109 = e6q09
     C                   eval      qt110 = e6q10
     C                   eval      qt111 = e6q11
     C                   eval      qt112 = e6q12
     C                   eval      qt113 = e6q13
     C                   eval      qt114 = e6q14
     C                   eval      qt115 = e6q15
     C                   eval      qt116 = e6q16
     C                   eval      qtmoac = e6qlan
     C                   eval      cudbac = e6cudb
     C                   eval      ccliac = e6ccli
     C                   eval      nordac = e6nord
     C                   eval      nrigac = e6nrig
     C                   eval      srigac = e6srig
     C                   eval      cfamac = e6cfam
     C                   eval      cdopac = 'I'

     C                   call      'MP004'
     C                   parm                    kpjba
     C                   parm                    zmp004
      *
     C                   if        critac <> *blanks
     C                   eval      $errore = 'E'
     C                   clear                   zbferr
     C                   eval      cstgab = e6cstg
     C                   eval      cudbab = e6cudb
     C                   eval      ccliab = e6ccli
     C                   eval      nordab = e6nord
     C                   eval      nrigab = e6nrig
     C                   eval      srigab = e6srig
     C                   eval      noprab = e6nopr
     C                   eval      roprab = e6ropr
     C                   eval      srprab = e6srpr
     C                   eval      ccpaab = e6ccpa
     C                   eval      msgeab = msgeac
     C                   exsr      Errore
     C                   endif
      *
     C                   enddo
     *
     C                   endsr
     *
     *****************************************************************
     *    Errore: Genero archivio errori
     *****************************************************************
     C     Errore        begsr
     *
     C                   eval      equdbr = ##cudb
     C                   eval      eqannr = S1anno
     C                   move      S1prtr        eqnric
     C                   eval      eqtipo = 'RIC'
     C     Kerrore       setgt     wpfer02k
     C     Kerrore       readpe    wpfer02k
     C                   if        %EOF(Wpfer02k)
     C                   eval      eqrigr = 1
     C                   else
     C                   eval      eqrigr = eqrigr + 1
     C                   endif
     *
     *
     C                   eval      eqmode = @PGM
     C                   eval      eqider = msgeab
     C                   clear                   msgeab
     C                   eval      eqdati = zbferr
     *
     C                   eval      a003eq = ##cudb
     C                   eval      a004eq = ##cudb
     C                   eval      a005eq = ##cudb
     C                   exsr      cabfic
     C                   write     rwpfer
     *
     C                   endsr
     *
     *****************************************************************
     * Salva_SNS01 Salvo i dati passatomi da CSIS
     *****************************************************************
     C     Salva_SNS01   BegSr
     *
     C                   clear                   rsnssv
     C                   eval      Fldbrs = S1dbrs
     C                   eval      Fldbow = S1dbow
     C                   eval      Fldbnx = S1dbnx
     C                   eval      Flflan = S1flan
     C                   eval      Flanno = S1anno
     C                   eval      Flprtr = S1prtr
     C                   eval      Flubor = S1ubor
     C                   eval      Fltdor = S1tdor
     C                   eval      Flpdor = S1pdor
     C                   eval      Flndor = S1ndor
     C                   eval      Flubpt = S1ubpt
     C                   eval      Flprot = S1prot
     C                   eval      Flnror = S1nror
     C                   eval      Flubus = S1ubus
     C                   eval      Fltdoc = S1tdoc
     C                   eval      Flpdoc = S1pdoc
     C                   eval      Flndoc = S1ndoc
     C                   eval      Flriga = S1riga
     C                   eval      Fldata = S1data
     C                   eval      Flq01  = S1qn01
     C                   eval      Flq02  = S1qn02
     C                   eval      Flq03  = S1qn03
     C                   eval      Flq04  = S1qn04
     C                   eval      Flq05  = S1qn05
     C                   eval      Flq06  = S1qn06
     C                   eval      Flq07  = S1qn07
     C                   eval      Flq08  = S1qn08
     C                   eval      Flq09  = S1qn09
     C                   eval      Flq10  = S1qn10
     C                   eval      Flq11  = S1qn11
     C                   eval      Flq12  = S1qn12
     C                   eval      Flq13  = S1qn13
     C                   eval      Flq14  = S1qn14
     C                   eval      Flq15  = S1qn15
     C                   eval      Flq16  = S1qn16
     C                   eval      Fltoqn = S1toqn
     C                   eval      Flcava = S1cava
     C                   eval      Flubmg = S1ubmg
     C                   eval      Flmaga = S1maga
     C                   eval      Fltspr = S1tspr
     C                   eval      Flstol = S1stol
     C                   eval      Flstnw = S1stnw
     *
     C                   eval      a003fl = ##cudb
     C                   eval      a004fl = ##cudb
     C                   eval      a005fl = ##cudb
     C                   exsr      cabfic
     C                   write     Rsnssv
     *
     C                   endsr
     *
     *
     *****************************************************************
     *    Pulisci_err: Pulizia file WPFERR
     *****************************************************************
     C     Pulisci_err   begsr
     *
     C                   eval      equdbr = ##cudb
     C                   eval      eqannr = S1anno
     C                   move      S1prtr        eqnric
     C                   eval      eqtipo = 'RIC'
     C     Kerrore       setll     wpfer02k
     C                   do        *hival
     C     Kerrore       reade     wpfer02k
     C                   if        %EOF
     C                   leave
     C                   else
     C                   delete    rwpfer
     C                   endif
     *
     C                   enddo
     *
     C                   endsr
     *
     *****************************************************************
     *    Trasco_tgl:: Trascodifico taglie
     *****************************************************************
     C     Trasco_tgl    begsr
     *
     C                   clear                   Zsy921
     C                   eval      cdopln = $trascotgl
     C                   eval      ttagln = $ttag
     C                   eval      qln01  = tgl01
     C                   eval      qln02  = tgl02
     C                   eval      qln03  = tgl03
     C                   eval      qln04  = tgl04
     C                   eval      qln05  = tgl05
     C                   eval      qln06  = tgl06
     C                   eval      qln07  = tgl07
     C                   eval      qln08  = tgl08
     C                   eval      qln09  = tgl09
     C                   eval      qln10  = tgl10
     C                   eval      qln11  = tgl11
     C                   eval      qln12  = tgl12
     C                   eval      qln13  = tgl13
     C                   eval      qln14  = tgl14
     C                   eval      qln15  = tgl15
     C                   eval      qln16  = tgl16
     C                   eval      Kpjbu = Zsy921
     C                   call      'SY921'
     C                   parm                    Kpjba
     C                   eval      Zsy921 = Kpjbu
     *
     *
     C                   endsr
     *****************************************************************
     *    DEFCAM- Definizione campi chiave e operazioni iniziali     *
     *****************************************************************
     C     defcam        begsr
     *
     C     *entry        plist
     C                   parm                    kpjba
     *
     C                   If        KNRAZ = 0
     C                   Eval      KNRAZ = *All'9'
     C                   Endif
     *
     C                   Movel(P)  @User         KNMUS
     C                   Clear                   Kpjbu
     *
     C     *dtaara       define                  ##udb
     C                   in        ##udb
     *
BET  C     ##cudb        chain     udbmf01k
     *
     C                   endsr
     *****************************************************************
     * Girodata   Routine di giro data                               *
     *****************************************************************
     C     Girodata      BegSr
     *
     C                   CALL      'ADP@DAT1'
     C                   PARM      $dat06        XXDAT6            6 0
     C                   PARM      $dat08        XXDAT8            8 0
     C                   PARM      0             XXDATV            6 0
     C                   PARM      $modo         XXDATM            2
     *
     C                   EndSr
     *****************************************************************
     * Ctl_decoTAB Controllo e decodifica tabelle                    *
     *****************************************************************
     C     Ctl_decotab   BegSr
     *
     C                   Movel(P)  $ctab         @DLI0
     C                   Movel(P)  $cele         @DLI1
     C                   Call      '@TAB'
     C                   Parm                    @DLI0             3            Codice
     C                   Parm                    @DLI1             6            Elemento
     C                   Parm                    @DLI2           150            Dati della
     C                   Parm      *Blanks       @DLI3             1            Errore '1'
     C                   Parm                    LINGUA            1            Lingua
     C                   Parm                    FAMILY            1            Famiglia
     *
     C                   EndSr
     *****************************************************************
     * Trasco_tabe Trascodifiche tabelle da CSIS a SNS               *
     *****************************************************************
     C     Trasco_tabe   BegSr
     *
     C                   Movel(P)  $ctab         @TSC0
     C                   Movel(P)  $cele         @TSC1
     C                   Call      '@TSC'
     C                   Parm                    @TSC0             3            Codice
     C                   Parm                    @TSC1             6            Elemento CSIS
     C                   Parm                    @TSC2             6            Elemento SNS
     C                   Parm                    FAMILY            1            Famiglia
     *
     C                   EndSr
     *****************************************************************
     * CABFIC
     *****************************************************************
     C     Cabfic        begsr
     C                   MOVE      @PGM          PPNPGM           10
     C                   TIME                    PPHORA            6 0
     C                   Z-ADD     KNRAZ         PPNJOB            9 0
     C                   Z-ADD     *DAY          PPFECH            8 0
     C     *MONTH        MULT      100           VAR14            14 0
     C                   ADD       VAR14         PPFECH
     C     *YEAR         MULT      10000         VAR14
     C                   ADD       VAR14         PPFECH
     C                   MOVEL     KNMUS         PPCUTE           10
     C                   MOVEL     ##CUDB        PPCUDB            3
     C                   ENDSR
     *****************************************************************
     *    FINE  - Fine Programma
     *****************************************************************
     C     fine          begsr
     *
     C                   if        $chiamaerr
     C                   move      S1anno        $anno
     C                   call      'BF001CL'
     C                   parm                    S1dbrs
     C                   parm                    $anno
     C                   parm      @PGM          numerodoc         8
     C                   parm      'RIC'         $tipo             3
     C                   endif
     *
     C                   seton                                        lr
     *
     C                   endsr
     *****************************************************************
     * Deflist
     *****************************************************************
     C     DefKlist      begsr
MMD  *
MMD  C     KAVprd3       klist
MMD  C                   kfld                    ICCUDB
MMD  C                   kfld                    ICCSTG
MMD  C                   kfld                    ICCCLI
MMD  C                   kfld                    ICNORD
MMD  C                   kfld                    ICNRIG
MMD  C                   kfld                    ICSRIG
MMD  C                   kfld                    ICNOPR
MMD  C                   kfld                    ICROPR
MMD  C                   kfld                    ICSRPR
MMD  C                   kfld                    ICCCPP
MMD  C                   kfld                    ICPROG
MMD  C                   kfld                    ICCFPD
MMD  *
MMD  C     Kfapes        klist
MMD  C                   kfld                    iccfam
MMD  C                   kfld                    iccfbp
MMD  C                   kfld                    ##cudb
MMD  *
MMD  C     Kfape1        klist
MMD  C                   kfld                    e6cfam
MMD  C                   kfld                    ibcfbp
MMD  C                   kfld                    ##cudb
MMD  *
MMD  C     Kfrnan        klist
MMD  C                   kfld                    e6cudf
MMD  C                   kfld                    e6cfor
MMD  *
MMD  C     Kdocum        klist
MMD  C                   kfld                    s1ubus
MMD  C                   kfld                    s1tdoc
MMD  C                   kfld                    s1pdoc
MMD  C                   kfld                    s1ndoc
     *
     C     Kriga         klist
     C                   kfld                    S1tdor
     C                   kfld                    S1ubor
     C                   kfld                    S1pdor
     C                   kfld                    S1ndor
     C                   kfld                    rigaord
     C                   kfld                    S1ubpt
     C                   kfld                    S1prot
     *
     *
     C     Koadrg        klist
     C                   kfld                    $ubor
     C                   kfld                    $tdor
     C                   kfld                    $pdor
     C                   kfld                    $ndor
     C                   kfld                    $nror
     *
     *
     C     Krigap        klist
     C                   kfld                    S1tdor
     C                   kfld                    S1ubor
     C                   kfld                    S1pdor
     C                   kfld                    S1ndor
     C                   kfld                    rigaord
     *
     C     Korcpa        klist
     C                   kfld                    aucudb
     C                   kfld                    aucstg
     C                   kfld                    auccli
     C                   kfld                    aunord
     C                   kfld                    aunrig
     C                   kfld                    ausrig
     *
     C     Korpra        klist
     C                   kfld                    aucudb
     C                   kfld                    aucstg
     C                   kfld                    auccli
     C                   kfld                    aunord
     C                   kfld                    aunrig
     C                   kfld                    ausrig
     C                   kfld                    e6nopr
     C                   kfld                    e6ropr
     C                   kfld                    e6srpr
     C                   kfld                    $ccpa
     *
     C     Kfile         klist
     C                   kfld                    S1ubus
     C                   kfld                    S1tdoc
     C                   kfld                    S1pdoc
     C                   kfld                    S1ndoc
     *
     C     Kovdrgbox     klist
     C                   kfld                    brubor
     C                   kfld                    brtdor
     C                   kfld                    brpdor
     C                   kfld                    brndor
     C                   kfld                    brnror
     *
     C     Kor28box      klist
     C                   kfld                    brtdor
     C                   kfld                    brubor
     C                   kfld                    brpdor
     C                   kfld                    brndor
     C                   kfld                    rigaord
     *
     C     Kor28lis      klist
     C                   kfld                    lrtdor
     C                   kfld                    lrubor
     C                   kfld                    lrpdor
     C                   kfld                    lrndor
     C                   kfld                    rigaord
     *
     C     Kor28ovr      klist
     C                   kfld                    motdor
     C                   kfld                    moubor
     C                   kfld                    mopdor
     C                   kfld                    mondor
     C                   kfld                    monror
     *
     C     Kbovrg        klist
     C                   kfld                    S1ubus
     C                   kfld                    S1tdoc
     C                   kfld                    S1pdoc
     C                   kfld                    S1ndoc
     C                   kfld                    S1riga
     *
     C     Kovdrglis     klist
     C                   kfld                    lrubor
     C                   kfld                    lrtdor
     C                   kfld                    lrpdor
     C                   kfld                    lrndor
     C                   kfld                    lrnror
BET  *
 "   C     Kmagan        klist
 "   C                   kfld                    ##cudb
BET  C                   kfld                    aumags
     *
     C     KorpraSNS     klist
     C                   kfld                    aucudb
     C                   kfld                    aucstg
     C                   kfld                    auccli
     C                   kfld                    aunord
     C                   kfld                    aunrig
     C                   kfld                    ausrig
     C                   kfld                    e6nopr
     C                   kfld                    e6ropr
     C                   kfld                    e6srpr
     *
     C     Kbovte        klist
     C                   kfld                    btubbx
     C                   kfld                    bttdbx
     C                   kfld                    btpdbx
     C                   kfld                    btndbx
     *
     C     KbovteSPE     klist
     C                   kfld                    tsubsp
     C                   kfld                    tstdsp
     C                   kfld                    tspdsp
     C                   kfld                    tsndsp
     *
     C     Kerrore       klist
     C                   kfld                    equdbr
     C                   kfld                    eqannr
     C                   kfld                    eqnric
     C                   kfld                    eqtipo
     *
     C     Kscacm        klist
     C                   kfld                    e6cudb
     C                   kfld                    e6cstg
     C                   kfld                    e6ccli
     C                   kfld                    e6nord
     C                   kfld                    e6nrig
     C                   kfld                    e6srig
     C                   kfld                    e6nopr
     C                   kfld                    e6ropr
     C                   kfld                    e6srpr
     *
     C     Kmagcsis      klist
     C                   kfld                    apcudb
     C                   kfld                    S1maga
     *
     C     Kconfbrg      klist
     C                   kfld                    c2ubbx
     C                   kfld                    c2tdbx
     C                   kfld                    c2pdbx
     C                   kfld                    c2ndbx
     C                   kfld                    c2nrbx
     *
     C     Kscacm02      klist
     C                   kfld                    aucfam
     C                   kfld                    aucstg
     C                   kfld                    wsnpac
     C                   kfld                    wsnpro
     *
     C     KS14te        klist
     C                   kfld                    goudbb
     C                   kfld                    gosbox
     C                   kfld                    goannb
     C                   kfld                    gonumb
     *
     C     KSSCCB        klist
     C                   kfld                    apcudb
     C                   kfld                    $seriebox
     C                   kfld                    goannb
     C                   kfld                    v5numb
     *
     C     Korcri        klist
     C                   kfld                    gpudbo
     C                   kfld                    gpcstg
     C                   kfld                    gpccli
     C                   kfld                    gpnord
     C                   kfld                    gpnrig
     C                   kfld                    gpsrig
     *
     C     Korcrx        klist
     C                   kfld                    e6cstg
     C                   kfld                    e6cudb
     C                   kfld                    e6ccli
     C                   kfld                    e6nord
     C                   kfld                    e6nrig
     C                   kfld                    e6srig
     *
     C     Kortri        klist
     C                   kfld                    $cudf
     C                   kfld                    $cfor
     C                   kfld                    e6cudb
     C                   kfld                    e6cstg
     C                   kfld                    e6ccli
     C                   kfld                    e6nord
     C                   kfld                    e6nrig
     C                   kfld                    e6srig
     C                   kfld                    e6nopr
     C                   kfld                    e6ropr
     C                   kfld                    e6srpr
     C                   kfld                    e6ccpa
     *
     C     Kmcocc        klist
     C                   kfld                    $udbpr
     C                   kfld                    $udbde
     *
     C     Krecorpra     klist
     C                   kfld                    cudbai
     C                   kfld                    cstgai
     C                   kfld                    ccliai
     C                   kfld                    nordai
     C                   kfld                    nrigai
     C                   kfld                    srigai
     C                   kfld                    noprai
     C                   kfld                    roprai
     C                   kfld                    srprai
     C                   kfld                    ccppai
     *
     C     Klprrg        klist
     C                   kfld                    lrubor
     C                   kfld                    lrtdor
     C                   kfld                    lrpdor
     C                   kfld                    lrndor
     C                   kfld                    lrnror
     C                   kfld                    aupubo
     C                   kfld                    auptdo
     C                   kfld                    auppdo
     C                   kfld                    aupndo
     *
     C     Klpr          klist
     C                   kfld                    S1ubus
     C                   kfld                    S1tdoc
     C                   kfld                    S1pdoc
     C                   kfld                    S1ndoc
     C                   kfld                    $rigalis
     *
 "   C     KORCPA1       KLIST
 "   C                   KFLD                    auCUDB
 "   C                   KFLD                    auCSTG
 "   C                   KFLD                    auCCLI
 "   C                   KFLD                    auNORD
 "   C                   KFLD                    auNRIG
 "   C                   KFLD                    auSRIG
 "   C                   KFLD                    $CVAR
 "   C                   KFLD                    MGPROG
 "   *
 "   C     KORCPA2       KLIST
 "   C                   KFLD                    auCUDB
 "   C                   KFLD                    auCSTG
 "   C                   KFLD                    auCCLI
 "   C                   KFLD                    auNORD
 "   C                   KFLD                    auNRIG
 "   C                   KFLD                    auSRIG
BET  C                   KFLD                    $CVAR
     *
     C     Kortrd        klist
     C                   kfld                    sdcstg
     C                   kfld                    sdcudb
     C                   kfld                    sdccli
     C                   kfld                    sdnord
     C                   kfld                    sdnrig
     C                   kfld                    sdsrig
     C                   kfld                    sdnopr
     C                   kfld                    sdropr
     C                   kfld                    sdsrpr
     C                   kfld                    sdccpp
     C                   kfld                    sdtnor
     C                   kfld                    ihndoc
     *
     C     Kovtcm        klist
     C                   kfld                    S1ubor
     C                   kfld                    S1tdor
     C                   kfld                    S1pdor
     C                   kfld                    S1ndor
     C                   kfld                    S1nror
     *
BET9 C     Kavprd18k     klist
 "   C                   kfld                    aucudb
 "   C                   kfld                    aucstg
 "   C                   kfld                    auccli
 "   C                   kfld                    aunord
 "   C                   kfld                    aunrig
 "   C                   kfld                    ausrig
 "   C                   kfld                    e6nopr
 "   C                   kfld                    e6ropr
 "   C                   kfld                    e6srpr
BET9 C**$                kfld                    e6ccpa                         **lel**
     *
     C     KMGDIF        klist
     C                   kfld                    ICCUDB
     C                   kfld                    ICCSTG
     C                   kfld                    ICCCLI
     C                   kfld                    ICNORD
     C                   kfld                    ICNRIG
     C                   kfld                    ICSRIG
     C                   kfld                    S1ANNO
     C                   kfld                    S1PRTR
     *
     C     ksns0101l     klist
     C                   kfld                    S1UBOR
     C                   kfld                    S1TDOR
     C                   kfld                    S1PDOR
     C                   kfld                    S1NDOR
     C                   kfld                    S1UBPT
     C                   kfld                    S1PROT
     C                   kfld                    S1NROR
     C                   kfld                    S1UBUS
     C                   kfld                    S1TDOC
     C                   kfld                    S1PDOC
     C                   kfld                    S1NDOC
     C                   kfld                    S1RIGA
     *
     C     KMPFAN        klist
MMD  C                   kfld                    ##cudb
     C                   kfld                    h5cman
     C                   kfld                    aucstg
     C                   kfld                    aucudb
     C                   kfld                    auccli
     C                   kfld                    aunord
     C                   kfld                    aunrig
     C                   kfld                    ausrig
     C                   kfld                    b1nuca
     C                   kfld                    auccpm
BET3 *
 "   C     KMPFAN20      klist
 "   C                   kfld                    ab1cstg
 "   C                   kfld                    ab1cudb
 "   C                   kfld                    ab1ccli
 "   C                   kfld                    ab1nord
 "   C                   kfld                    ab1nrig
 "   C                   kfld                    ab1srig
BET3 C                   kfld                    ##cudb
     *
     C     kavprd03      Klist
     C                   Kfld                    E6CUDB
     C                   Kfld                    E6CSTG
     C                   Kfld                    E6CCLI
     C                   Kfld                    E6NORD
     C                   Kfld                    E6NRIG
     C                   Kfld                    E6SRIG
     C                   Kfld                    E6NOPR
     C                   Kfld                    E6ROPR
     C                   Kfld                    E6SRPR
     C                   Kfld                    E6CCPP
     C                   Kfld                    ICPROG
     *
     C     kavprd03x     Klist
     C                   Kfld                    E6CUDB
     C                   Kfld                    E6CSTG
     C                   Kfld                    E6CCLI
     C                   Kfld                    E6NORD
     C                   Kfld                    E6NRIG
     C                   Kfld                    E6SRIG
     C                   Kfld                    E6NOPR
     C                   Kfld                    E6ROPR
     C                   Kfld                    E6SRPR
     C                   Kfld                    E6CCPP
     *
     C     kavprd03cp    Klist
     C                   Kfld                    e6CUDB
     C                   Kfld                    e6CSTG
     C                   Kfld                    e6CCLI
     C                   Kfld                    e6NORD
     C                   Kfld                    e6NRIG
     C                   Kfld                    e6SRIG
     C                   Kfld                    e6NOPR
     C                   Kfld                    e6ROPR
     C                   Kfld                    e6SRPR
     C                   Kfld                    $$CCPP
     *
     C     kflufa        Klist
     C                   Kfld                    e6cfam
     C                   Kfld                    e6cdst
     C                   Kfld                    h9cfpd
     *
     C     Korcrim       klist
     C                   kfld                    AUCFAM
     C                   kfld                    CMOD
     C                   kfld                    CTEC
     C                   kfld                    COLC
     C                   kfld                    AUCSTG
     *
PG1  c     Klegor        klist
 "   c                   kfld                    $$CUDB
 "   c                   kfld                    $$CSTG
 "   c                   kfld                    $$CCLI
 "   c                   kfld                    $$NORD
 "   c                   kfld                    $$NRIG
 "   c                   kfld                    $$SRIG
 "   c                   kfld                    AUCUDB
 "   c                   kfld                    AUCSTG
 "   c                   kfld                    AUCCLI
 "   c                   kfld                    AUNORD
 "   c                   kfld                    AUNRIG
 "   c                   kfld                    AUSRIG
 "   *
 "   c     KlegAss       klist
 "   c                   kfld                    U1UDBL
 "   c                   kfld                    U1STGL
 "   c                   kfld                    U1CLIL
 "   c                   kfld                    U1ORDL
 "   c                   kfld                    U1RIGL
 "   c                   kfld                    U1SRGL
 "   c                   kfld                    U1CUD1
 "   c                   kfld                    U1CST1
 "   c                   kfld                    U1CCL1
 "   c                   kfld                    U1NOR1
 "   c                   kfld                    U1NRI1
 "   c                   kfld                    U1SRI1
 "   *
 "   c     KImbor        klist
 "   c                   kfld                    ##cudb
 "   c                   kfld                    xO5SBOX
 "   c                   kfld                    xO5ANNB
 "   c                   kfld                    xO5NUMB
 "   c                   kfld                    xO5NRGB
 "    *
 "   c     KorcriBox     klist
 "   c                   kfld                    O5CUDB
 "   c                   kfld                    O5CSTG
 "   c                   kfld                    O5CCLI
 "   c                   kfld                    O5NORD
 "   c                   kfld                    O5NRIG
 "   *
 "   c     KBoarg        klist
 "   c                   kfld                    S1UBOR
 "   c                   kfld                    S1TDOR
 "   c                   kfld                    S1PDOR
 "   c                   kfld                    S1NDOR
 "   c                   kfld                    S1NROR
 "   *
 "   c     KOrcriBoa     klist
 "   c                   kfld                    BQTDOR
 "   c                   kfld                    BQUBOR
 "   c                   kfld                    BQPDOR
 "   c                   kfld                    BQNDOR
PG1  c                   kfld                    BQNROR
     *
     C                   endsr
BET  *****************************************************************
 "   C     Savorcri      begsr
 "   *****************************************************************
 "    * Salvataggio Campi ORCRIAUF per Scrittura ORCVRFRF
 "   C                   MOVE      AUCUDB        CUDBGF
 "   C                   MOVE      AUCSTG        CSTGGF
 "   C                   MOVE      AUCCLI        CCLIGF
 "   C                   MOVE      AUNORD        NORDGF
 "   C                   MOVE      AUNRIG        NRIGGF
 "   C                   MOVE      AUSRIG        SRIGGF
     c                   if        auctec=*blanks
 "   C                   MOVE      AUCTES        CTESGF
 "   C                   MOVE      AUCOL3        CO3TGF
     c                   else
 "   C                   MOVE      AUCTEc        CTESGF
 "   C                   MOVE      AUCOLc        CO3TGF
     c                   endif
 "   C                   MOVE      AUCTEC        CTECGF
 "   C                   MOVE      AUCOLC        COLCGF
 "   C                   MOVE      AUCMOD        CMODGF
 "   C                   MOVE      AUCFPT        CFPTGF
 "   C                   MOVE      AUCFPM        CFPMGF
 "   C                   MOVE      AUCFOD        CFODGF
 "   C                   MOVE      AUCOLF        CO3FGF
 "   C                   MOVE      AUCBOT        CBOTGF
 "   C                   MOVE      AUCOLB        CO3BGF
 "   C                   MOVE      AUCVFP        CVFPGF
 "   C                   MOVE      AUCIMP        CIMPGF
 "   C                   MOVE      AUTASO        TASOGF
 "   C                   MOVE      AUTSCT        TSCTGF
 "   C                   MOVE      AUCCCT        CCCTGF
 "   C                   MOVE      AUCCIP        CCIPGF
 "   C                   MOVE      AUCSER        CSERGF
 "   C                   MOVE      AUCCMP        CCMPGF
 "   C                   MOVE      AUSTAT        STATGF
 "   C                   MOVE      AUCDRO        CDROGF
 "   C                   MOVE      AUCSTA        CSTAGF
 "   C                   MOVE      AUPRZE        PRZEGF
 "   C                   MOVE      AUPSCR        PSCRGF
 "   C                   MOVE      AUVSCR        VSCRGF
 "   C                   MOVE      AUVSRE        VSREGF
 "   C                   MOVE      @PGM          PGMVGF
 "   C                   MOVE      AUCMOS        CMOSGF
 "   C                   MOVE      AUTESO        TESOGF
 "   C                   MOVE      AUCOLO        CO3SGF
 "   C                   MOVE      AUCTE2        CTE2GF
 "   C                   MOVE      AUCLT2        CO32GF
 "   C                   MOVE      AUCdst        cdstGF
 "   *
 "   C                   endsr
BET  *****************************************************************
 "   C     Savorcrim     begsr
 "   *****************************************************************
 "    * Salvataggio Campi ORCRIM0K per Scrittura ORCVRFRF
 "   C                   MOVE      M_AUCUDB      CUDBGF
 "   C                   MOVE      M_AUCSTG      CSTGGF
 "   C                   MOVE      M_AUCCLI      CCLIGF
 "   C                   MOVE      M_AUNORD      NORDGF
 "   C                   MOVE      M_AUNRIG      NRIGGF
 "   C                   MOVE      M_AUSRIG      SRIGGF
     c                   if        M_auctec=*blanks
 "   C                   MOVE      M_AUCTES      CTESGF
 "   C                   MOVE      M_AUCOL3      CO3TGF
     c                   else
 "   C                   MOVE      M_AUCTEc      CTESGF
 "   C                   MOVE      M_AUCOLc      CO3TGF
     c                   endif
 "   C                   MOVE      M_AUCTEC      CTECGF
 "   C                   MOVE      M_AUCOLC      COLCGF
 "   C                   MOVE      M_AUCMOD      CMODGF
 "   C                   MOVE      M_AUCFPT      CFPTGF
 "   C                   MOVE      M_AUCFPM      CFPMGF
 "   C                   MOVE      M_AUCFOD      CFODGF
 "   C                   MOVE      M_AUCOLF      CO3FGF
 "   C                   MOVE      M_AUCBOT      CBOTGF
 "   C                   MOVE      M_AUCOLB      CO3BGF
 "   C                   MOVE      M_AUCVFP      CVFPGF
 "   C                   MOVE      M_AUCIMP      CIMPGF
 "   C                   MOVE      M_AUTASO      TASOGF
 "   C                   MOVE      M_AUTSCT      TSCTGF
 "   C                   MOVE      M_AUCCCT      CCCTGF
 "   C                   MOVE      M_AUCCIP      CCIPGF
 "   C                   MOVE      M_AUCSER      CSERGF
 "   C                   MOVE      M_AUCCMP      CCMPGF
 "   C                   MOVE      M_AUSTAT      STATGF
 "   C                   MOVE      M_AUCDRO      CDROGF
 "   C                   MOVE      M_AUCSTA      CSTAGF
 "   C                   MOVE      M_AUPRZE      PRZEGF
 "   C                   MOVE      M_AUPSCR      PSCRGF
 "   C                   MOVE      M_AUVSCR      VSCRGF
 "   C                   MOVE      M_AUVSRE      VSREGF
 "   C                   MOVE      @PGM          PGMVGF
 "   C                   MOVE      M_AUCMOS      CMOSGF
 "   C                   MOVE      M_AUTESO      TESOGF
 "   C                   MOVE      M_AUCOLO      CO3SGF
 "   C                   MOVE      M_AUCTE2      CTE2GF
 "   C                   MOVE      M_AUCLT2      CO32GF
 "   C                   MOVE      M_AUCdst      cdstGF
 "   *
 "   C                   endsr
 "   *****************************************************************
 "   * Svariaz - registra variazione su ORCVR                        *
 "   *****************************************************************
 "   C     Svariaz       begsr
 "    ** Salvo KPJBU
 "   C                   MOVEL     KPJBU         $$KPJBU
     c                   eval      INVCGF = 'Y'
 "    ** Muove dati per scrittura record ORCVR
 "   C*****              MOVEL     ZVE500        KPJBU
 "   C                   CALL      'VE500'
 "   C                   PARM                    KPJBA
 "   C                   PARM                    zve500
 "    ** Ripristina KPJBU
 "   C                   MOVEL     $$KPJBU       KPJBU
 "   *
 "   C                   endsr
 "
 "   *****************************************************************
 "   * RiportaPremag - Riporta riga a Premagazzino                   *
 "   *****************************************************************
 "   C     RiportaPremag begsr
 "   *
 "   C     Kriga         chain     orcri28k
 "   C                   if        not %found
 "   C     Krigap        chain     orcri28k
 "   C                   if        not %found
 "   C                   eval      $errore = 'S'
 "   C                   clear                   zbferr
 "   C                   eval      uborab = S1ubor
 "   C                   eval      tdorab = S1tdor
 "   C                   eval      pdorab = S1pdor
 "   C                   eval      ndorab = S1ndor
 "   C                   eval      ubptab = S1ubpt
 "   C                   eval      protab = S1prot
 "   C                   eval      nrorab = S1nror
 "   C                   eval      msgeab = 'SNS8145'
 "   C                   exsr      Errore
 "   C                   eval      S1flan = 'E'
 "   C                   LeaveSr
 "   C                   endif
 "   C                   endif
 "
     c                   if        austan > '200'
     C                   eval      $errore = 'E'
     C                   clear                   zbferr
     C                   eval      uborab = S1ubor
     C                   eval      tdorab = S1tdor
     C                   eval      pdorab = S1pdor
     C                   eval      ndorab = S1ndor
     C                   eval      ubptab = S1ubpt
     C                   eval      protab = S1prot
     C                   eval      nrorab = S1nror
     C                   eval      statab = austat
     C                   eval      msgeab = 'SNS2725'
     C                   exsr      Errore
     C                   Leavesr
     c                   endif
 "
 "   C                   CLEAR                   ZVE500
 "   C                   ExSr      Savorcri
 "
 "   *
 "   * Aggiorno campi di ORCRI
 "   * metto qt{ versata = 0
 "   c                   eval      aucqtm=0
 "   c                   eval      aum01=0
 "   c                   eval      aum02=0
 "   c                   eval      aum03=0
 "   c                   eval      aum04=0
 "   c                   eval      aum05=0
 "   c                   eval      aum06=0
 "   c                   eval      aum07=0
 "   c                   eval      aum08=0
 "   c                   eval      aum09=0
 "   c                   eval      aum10=0
 "   c                   eval      aum11=0
 "   c                   eval      aum12=0
 "   c                   eval      aum13=0
 "   c                   eval      aum14=0
 "   c                   eval      aum15=0
 "   c                   eval      aum16=0
 "   c                   eval      austat='TG'
 "   c     austat        chain     evean01k
 "   c                   if        %found
 "   c                   eval      austan=cgstan
 "   c                   endif
 "   c                   Z-ADD     $$dat8        audtst
 "   c                   eval      audtma=0
 "   *
 "   c                   exsr      cabfic
 "   c                   update    rorcri
 "   c                   exsr      Svariaz
 "
 "   *
 "   * Aggiorno campi di ORPRA
 "   *
 "   C                   eval      e6nopr = aunord
 "   C                   eval      e6ropr = aunrig
 "   C                   eval      e6srpr = ausrig
 "   c     korpraSNS     Setll     ORPRA01K
 "   c                   Do        *HIVAL
 "
 "   c     korpraSNS     Reade     ORPRA01K
 "   c                   If        %EOF
 "   c                   Leave
 "   c                   EndIf
     C                   eval      $controllo = *ON
 "
     * Verifico se e' un componente della parte di capo, se si la scarto
     C     e6ccpa        chain     cpman01k
     c                   IF        %found(cpman01k)
     c                             and acfgcm = 'Y'
     c                   iter
     c                   endif
     *
 "   c                   Eval      e6stat='MC'
 "   c     E6stat        Chain     evean01k
 "   c                   If        %found
 "   c                   Eval      E6stan=cgstan
 "   c                   EndIf
 "   c                   ExSr      cabfic
 "   c                   Update    orpra01
 "
 "   c                   EndDo
 "
 "   * Carico PREMAGAZZINO
 "   c                   exsr      Cacamag
 "   *
 "   c                   endsr
 "   *****************************************************************
 "   * Carico Premagazzino
 "   *****************************************************************
 "   c     Cacamag       begsr
 "   c     ##CUDB        Chain     udbmf01k
 "   c                   Clear                   ZMP004
 "   * Rep.mag.principale (BZCMAG):
 "   c     kmagan        chain     magan05k
 "   c                   Movel(P)  bzmagp        cmagac                         Causale movimento
 "   *
 "   c                   Movel(P)  'CAM'         $ctab
 "   c                   Movel(P)  h5trpm        $cele
 "   c                   Exsr      Ctl_decotab
 "   c                   If        @DLI3 = '1'
 "   c                   eval      $errore = 'E'
 "   c                   clear                   zbferr
 "   c                   eval      uborab = S1ubor
 "   c                   eval      tdorab = S1tdor
 "   c                   eval      pdorab = S1pdor
 "   c                   eval      ndorab = S1ndor
 "   c                   eval      ubptab = S1ubpt
 "   c                   eval      protab = S1prot
 "   c                   eval      nrorab = S1nror
 "   c                   eval      msgeab = 'SNS8384'
 "   c                   exsr      Errore
 "   c                   eval      S1flan = 'E'
 "   c                   LeaveSr
 "   c                   EndIf
 "   c                   Movel(P)  @dli2         z@cmmg
 "   c                   Eval      CMMGAC = CAUSB1
 "
 "   *
 "   * Carico a PREMAGAZZINO
 "   *
 "   c                   exsr      Scardat
 "   *
 "   c                   Call      'MP004'
 "   c                   Parm                    KPJBA
 "   c                   Parm                    zmp004
 "
 "   c     Critac        IfNE      *blanks
 "   c                   eval      $errore = 'E'
 "   c                   clear                   zbferr
 "   c                   eval      uborab = S1ubor
 "   c                   eval      tdorab = S1tdor
 "   c                   eval      pdorab = S1pdor
 "   c                   eval      ndorab = S1ndor
 "   c                   eval      ubptab = S1ubpt
 "   c                   eval      protab = S1prot
 "   c                   eval      nrorab = S1nror
 "   c                   eval      msgeab = msgeac
 "   c                   exsr      Errore
 "   c                   eval      S1flan = 'E'
 "   c                   LeaveSr
 "   c                   EndIf
 "   *
 "   * Creo Nuovo ricevimento
 "   *
 "   c                   Clear                   zbf011
 "   c                   MOVEL     'Y'           FRITFM
 "   c                   MOVEL     e6cstg        CSTGFM
 "   c                   MOVEL     E6CUDB        CUDBFM
 "   c                   MOVEL     E6CCLI        CCLIFM
 "   c                   MOVE      E6NORD        NORDFM
 "   c                   MOVE      E6NRIG        NRIGFM
 "   c                   MOVE      E6SRIG        SRIGFM
 "   c                   MOVE      E6NOPR        NOPRFM
 "   c                   MOVE      E6ROPR        ROPRFM
 "   c                   MOVE      E6SRPR        SRPRFM
 "   c                   MOVE      E6CCPA        CCPAFM
 "    *
 "    *  Quantita
 "   c                   Z-add     aua01         QT01
 "   c                   Z-add     aua02         QT02
 "   c                   Z-add     aua03         QT03
 "   c                   Z-add     aua04         QT04
 "   c                   Z-add     aua05         QT05
 "   c                   Z-add     aua06         QT06
 "   c                   Z-add     aua07         QT07
 "   c                   Z-add     aua08         QT08
 "   c                   Z-add     aua09         QT09
 "   c                   Z-add     aua10         QT10
 "   c                   Z-add     aua11         QT11
 "   c                   Z-add     aua12         QT12
 "   c                   Z-add     aua13         QT13
 "   c                   Z-add     aua14         QT14
 "   c                   Z-add     aua15         QT15
 "   c                   Z-add     aua16         QT16
 "   c                   Z-add     aucqta        QTGAFM
 "    *
 "   c                   MOVEL     '*'           TIVPFM
 "    *
 "   c                   Movel(P)  ZBF011        Kpjbu
 "   c                   CALL      'BF011L'
 "   c                   PARM                    Kpjba
      *  Aggiorno avanzamento produttivo
 "   C                   Clear                   zsn680
 "   C                   Eval      $_pgmci2 = 'BF003L'
 "   C                   Eval      $_FICOI2 = 'Y'
 "   C                   Eval      $_GECOI2 = 'N'
 "   C                   Eval      $_CUDBI2 = aucudb
 "   C                   Eval      $_CCLII2 = auccli
 "   C                   Eval      $_CSTGI2 = aucstg
 "   C                   Eval      $_NORDI2 = aunord
 "   C                   Eval      $_NRIGI2 = aunrig
 "   C                   Eval      $_SRIGI2 = ausrig
 "   C                   Eval      $_NOPRI2 = aunord
 "   C                   Eval      $_ROPRI2 = aunrig
 "   C                   Eval      $_SRPRI2 = ausrig
 "
 "   C                   Eval      $_QTATI2 = aucqta
 "   C                   Eval      $_QTA01  = aua01
 "   C                   Eval      $_QTA02  = aua02
 "   C                   Eval      $_QTA03  = aua03
 "   C                   Eval      $_QTA04  = aua04
 "   C                   Eval      $_QTA05  = aua05
 "   C                   Eval      $_QTA06  = aua06
 "   C                   Eval      $_QTA07  = aua07
 "   C                   Eval      $_QTA08  = aua08
 "   C                   Eval      $_QTA09  = aua09
 "   C                   Eval      $_QTA10  = aua10
 "   C                   Eval      $_QTA11  = aua11
 "   C                   Eval      $_QTA12  = aua12
 "   C                   Eval      $_QTA13  = aua13
 "   C                   Eval      $_QTA14  = aua14
 "   C                   Eval      $_QTA15  = aua15
 "   C                   Eval      $_QTA16  = aua16
 "
 "   *  Carico la fase di premagazzino co capi gia accoppiati
 "   C                   Eval      $_CCPPI2 = e6ccpp
 "   C                   Eval      $_CCP1I2 = auccpm
 "   C                   Eval      $_CPA1I2 = e6ccpa                            **LEL**
 "   C                   Eval      $_UDB1I2 = ##cudb
 "   C                   Eval      $_CFP1I2 = h5fpre
 "   C                   Eval      $_TOP1I2 = 'CAR'
 "   C                   Eval      $kpjbu   = kpjbu
 "   C                   Eval      kpjbu    = zsn680
 "
 "   C                   Call      'SN680'
 "   C                   Parm                    kpjba
 "
 "   C                   Eval      zsn680   = kpjbu
 "   C                   Eval      kpjbu    = $kpjbu
 "   C                   If        $_criti2 = 'ER'
 "   C*                  eval      $errore = 'E'
 "   C                   clear                   zbferr
 "   C                   eval      cstgab = aucstg
 "   C                   eval      cudbab = aucudb
 "   C                   eval      ccliab = auccli
 "   C                   eval      nordab = aunord
 "   C                   eval      nrigab = aunrig
 "   C                   eval      srigab = ausrig
 "   C                   eval      ccpaab = auccpm
 "   C                   eval      msgeab = $_cmsgi2
 "   C*                  exsr      Errore
 "   C*                  leave
BET  C                   EndIf
 "   *
 "   c                   EndSr
 "   * ---------------------------------------------------------------- *
 "   * Carica dati per call a MP004
 "   * ---------------------------------------------------------------- *
 "   c     Scardat       begsr
 "   *
 "   c                   Z-ADD     aucqta        QTMOAC                         Numero capi
 "    *  Carico quantita' da movimentare per taglie
 "   c                   Z-ADD     aua01         QT101                          Capi per taglia
 "   c                   Z-ADD     aua02         QT102
 "   c                   Z-ADD     aua03         QT103
 "   c                   Z-ADD     aua04         QT104
 "   c                   Z-ADD     aua05         QT105
 "   c                   Z-ADD     aua06         QT106
 "   c                   Z-ADD     aua07         QT107
 "   c                   Z-ADD     aua08         QT108
 "   c                   Z-ADD     aua09         QT109
 "   c                   Z-ADD     aua10         QT110
 "   c                   Z-ADD     aua11         QT111
 "   c                   Z-ADD     aua12         QT112
 "   c                   Z-ADD     aua13         QT113
 "   c                   Z-ADD     aua14         QT114
 "   c                   Z-ADD     aua15         QT115
 "   c                   Z-ADD     aua16         QT116
 "   c                   MOVEL     auCUDB        CUDBAC                         Udb Ordine
 "   c                   MOVEL     auCSTG        CSTGAC                         Stagione
 "   c                   MOVEL     auCCLI        CCLIAC                         Cliente
 "   c                   Z-ADD     auNORD        NORDAC                         Ordine
 "   c                   Z-ADD     auNRIG        NRIGAC                         Riga
 "   c                   Z-ADD     auSRIG        SRIGAC                         Sottoriga
 "   c                   MOVEL     auCFAM        CFAMAC                         Marca
 "   c                   Movel     auccpm        ccpmac                         Tipo prodotto
 "   c                   MOVEL     ##CUDB        UDBLAC                         Udb lavoro
 "   c                   MOVEL     'Y'           MUDBAC
 "   c                   MOVEL     ##CUDB        UDBOAC
 "   c                   MOVEL     @PGM          NPGMAC
 "   c                   MOVE      'I'           CDOPAC                         Tipo opera
 "    * ---- --
 "    * NON OBBLIGATORI
 "   c                   MOVEL     auCMOD        CMODAC                         Modello
 "    *
 "    *  Forzo la proprieta' dell'ordine.
 "    *  Non movimento i vecchi archivi magazzini in INCO
 "   c                   MOVEL     'N'           FINCAC                         Mov.INCBOX
 "   c                   MOVEL     'N'           FMOMAC                         Mov.MOMAG
 "   c                   MOVEL     'N'           FMRIAC                         Mov.MAGRI
 "   *
 "    * Mag. di provenienza:
 "   c                   MOVEL     ##cudb        UDMPAC                         Udb mag.pr
 "   c                   MOVEL     'WMS'         MAGPAC
 "    * Mag. di destino  :
 "   c                   MOVEL     ##cudb        UDMDAC                         Udb mag.pr
 "   c                   MOVEL     bzmagp        MAGDAC                         Magazzino
 "   *
 "   c                   endsr
BET
 "   * ---------------------------------------------------------------- *
 "   * Test_OADRG: Se OADRG non esiste, verifico se e' un cambio cliente,
 "   *           : se non e' neanche quello scarto il record
 "   * ---------------------------------------------------------------- *
 "   c     Test_OADRG    begsr
 "   *
 "   c     Koadrg        chain     Oadrg51l
     c                   if        not %found(Oadrg51l)
     C     Koadrg        chain     Ovrrg02l
     c                   if        not %found(Ovrrg02l)
     c                   eval      $iter = 'Y'
     c                   else
     c                   endif
     c                   endif
 "   *
 "   c                   endsr
BET
 "   * ---------------------------------------------------------------- *
 "   * Annullo_PROP: Annullo Proposta spedizione
 "   * ---------------------------------------------------------------- *
 "   c     Annullo_prop  begsr
 "   *
     C     Kfile         setll     orcri0Bk
     C                   do        *hival
     C     Kfile         reade     orcri0Bk
     C                   if        %EOF
     C                   leave
     C                   endif
     C                   eval      $controllo = *ON
     C                   eval      aupubo = *blanks
     C                   eval      auptdo = *blanks
     C                   eval      auppdo = *blanks
     C                   eval      aupndo = *blanks
     C                   if        austat = 'SL'
     C                   eval      audtst = $$dat8
     C                   eval      austat = 'MA'
     C     austat        chain     evean01k
     C                   if        %found
     C                   eval      austan = cgstan
     C                   endif
     C                   endif
     C                   exsr      cabfic
     C                   update    orcri0B
     C                   enddo
 "   *
 "   c                   endsr
BET
 "   * ---------------------------------------------------------------- *
 "   * Annullo_DDT: Annullo DDT
 "   * ---------------------------------------------------------------- *
 "   c     Annullo_DDT   begsr
 "   *
     C     Kfile         setll     orcri0Ck
     C                   do        *hival
     C     Kfile         reade     orcri0Ck
     C                   if        %EOF
     C                   leave
     C                   endif
     C                   eval      $controllo = *ON
     C                   if        austat = 'SP'
     C                             or austat = 'TR'
     C                   eval      audubo = *blanks
     C                   eval      audtdo = *blanks
     C                   eval      audpdo = *blanks
     C                   eval      audndo = *blanks
     C                   eval      audtbo = *zeros
     C                   eval      audtst = $$dat8
     C                   eval      austat = 'SL'
     C     austat        chain     evean01k
     C                   if        %found
     C                   eval      austan = cgstan
     C                   endif
     C                   exsr      cabfic
     C                   update    orcri0C
     C                   endif
     *
     C                   enddo
 "   c                   endsr
BET
     *****************************************************************
     *    Preann_lel : Preannullamento C/L capi non consegnati
     *****************************************************************
     C     Preann_lel    begsr
 "   *
     C                   MOVEL     ' '           $PRE              1
 "   *
 "   *--Pulisco il file dei preannullamenti non elaborati, in quanto la quantit{ Y1
 "   *--E' il netto tra l'essere e il consegnato
     C     KORCPA        SETLL     ORCPA01K
     C                   DO        *HIVAL
     C     KORCPA        READE     ORCPA01K
     C                   if        %EOF(orcpa01k)
     C                   leave
     C                   endif
     C                   if        mgfela <> 'Y'
     C                   delete    rorcpa
     C                   endif
     C                   endDO
     *
     *--Azzero quanti{ preannullate
     C                   eval      AUP01  = 0
     C                   eval      AUP02  = 0
     C                   eval      AUP03  = 0
     C                   eval      AUP04  = 0
     C                   eval      AUP05  = 0
     C                   eval      AUP06  = 0
     C                   eval      AUP07  = 0
     C                   eval      AUP08  = 0
     C                   eval      AUP09  = 0
     C                   eval      AUP10  = 0
     C                   eval      AUP11  = 0
     C                   eval      AUP12  = 0
     C                   eval      AUP13  = 0
     C                   eval      AUP14  = 0
     C                   eval      AUP15  = 0
     C                   eval      AUP16  = 0
     C                   eval      AUCQTP = 0
 "   *
 "   *--Scrivo preannullamenti solo se non } bulk e le quantit{ sono differenti
     C**$                IF        aucqta > aucqtm AND aucblk =  *blanks
     C                   IF        aucblk = *blanks and AUCQTA > S1TOQN
 "   *
 "   C                   EVAL      $CVAR = 'Y1'
 "   C                   MOVE      *ALL'9'       MGPROG
 "   C     KORCPA1       SETGT     ORCPA01K
 "   C     KORCPA2       READPE    ORCPA01K
 "   C                   IF        %EOF(ORCPA01K)
 "   C                   Z-ADD     *ZEROS        $PROG
 "   C                   ELSE
 "   C                   EVAL      $PROG=MGPROG+1
 "   C                   UNLOCK    ORCPA01K
 "   C                   ENDIF
 "   *
 "   C                   CLEAR                   RORCPA
 "   *
 "   C                   EVAL      MGPROG = $PROG
 "   C                   EVAL      A003MG = ##CUDB
 "   C                   EVAL      A004MG = ##CUDB
 "   C                   EVAL      A005MG = ##CUDB
 "   C                   EVAL      MGCUDB = AUCUDB
 "   C                   EVAL      MGCSTG = AUCSTG
 "   C                   EVAL      MGCCLI = AUCCLI
 "   C                   EVAL      MGNORD = AUNORD
 "   C                   EVAL      MGNRIG = AUNRIG
 "   C                   EVAL      MGSRIG = AUSRIG
 "   C                   EVAL      MGCVAR = $CVAR
     *
 "   C                   EVAL      MGC01 = AUA01 - rlns(1)
 "   C                   EVAL      MGC02 = AUA02 - rlns(2)
 "   C                   EVAL      MGC03 = AUA03 - rlns(3)
 "   C                   EVAL      MGC04 = AUA04 - rlns(4)
 "   C                   EVAL      MGC05 = AUA05 - rlns(5)
 "   C                   EVAL      MGC06 = AUA06 - rlns(6)
 "   C                   EVAL      MGC07 = AUA07 - rlns(7)
 "   C                   EVAL      MGC08 = AUA08 - rlns(8)
 "   C                   EVAL      MGC09 = AUA09 - rlns(9)
 "   C                   EVAL      MGC10 = AUA10 - rlns(10)
 "   C                   EVAL      MGC11 = AUA11 - rlns(11)
 "   C                   EVAL      MGC12 = AUA12 - rlns(12)
 "   C                   EVAL      MGC13 = AUA13 - rlns(13)
 "   C                   EVAL      MGC14 = AUA14 - rlns(14)
 "   C                   EVAL      MGC15 = AUA15 - rlns(15)
 "   C                   EVAL      MGC16 = AUA16 - rlns(16)
     *
 "   C                   EVAL      MGCQTA = AUCQTA - s1toqn
     *
BET9 C                   IF        fnobia = 'Y'
     c                   exsr      sFasUltAVPRD
 "   C                   EVAL      MGGRPF = $ICGRPF
     C                   ENDIF
     *
 "   C                   EXSR      CABFIC
 "   C                   Z-ADD     $$DAT8        MGDATA
 "   C                   MOVEL     'N'           MGFELA
 "   C                   MOVEL     @USER         MGUSER
     *
 "   C                   EXSR      CABFIC
 "   C                   WRITE     RORCPA
     *
     C                   MOVEL     '*'           $PRE
 "   *
     *--Compila quantit{ preannullata su ORCRI
     C                   eval      AUP01  = MGC01
     C                   eval      AUP02  = MGC02
     C                   eval      AUP03  = MGC03
     C                   eval      AUP04  = MGC04
     C                   eval      AUP05  = MGC05
     C                   eval      AUP06  = MGC06
     C                   eval      AUP07  = MGC07
     C                   eval      AUP08  = MGC08
     C                   eval      AUP09  = MGC09
     C                   eval      AUP10  = MGC10
     C                   eval      AUP11  = MGC11
     C                   eval      AUP12  = MGC12
     C                   eval      AUP13  = MGC13
     C                   eval      AUP14  = MGC14
     C                   eval      AUP15  = MGC15
     C                   eval      AUP16  = MGC16
     C                   eval      AUCQTP = MGCQTA
     C                   exsr      cabfic
     C                   update    rorcri
     *
     *--Attualizzo i preannullamenti
     C                   CLEAR                   ZVE558
     C                   MOVEL     AuCSTG        CSTGBB
     C                   MOVEL     auCUDB        CUDBBB
     C                   MOVEL     auCCLI        CCLIBB
     C                   Z-ADD     auNORD        NORDBB
     C                   Z-ADD     auNRIG        NRIGBB
     C                   Z-ADD     auSRIG        SRIGBB
     C                   MOVEL     '*'           FAUTBB
BET2 C                   MOVEL     ZVE558        KPJBU
 "   C                   CALL      'VE558B'
 "   C                   PARM                    KPJBA
 "   C                   IF        CRITBB <> *BLANKS
     C                   RolBk
     C                   eval      $errore = 'E'
     C                   clear                   zbferr
     C                   eval      uborab = S1ubor
     C                   eval      tdorab = S1tdor
     C                   eval      pdorab = S1pdor
     C                   eval      ndorab = S1ndor
     C                   eval      ubptab = S1ubpt
     C                   eval      protab = S1prot
     C                   eval      nrorab = S1nror
     C                   eval      cstgab = aucstg
     C                   eval      cudbab = aucudb
     C                   eval      ccliab = auccli
     C                   eval      nordab = aunord
     C                   eval      nrigab = aunrig
     C                   eval      srigab = ausrig
     C                   eval      noprab = e6nopr
     C                   eval      roprab = e6ropr
     C                   eval      srprab = e6srpr
     C                   eval      ccpaab = $ccpa
     C                   eval      stapab = e6stat
     C                   eval      msgeab = MSGEBB
     C                   exsr      Errore
     C                   LeaveSr
BET2 C                   EndIf
 "   **
     C                   Else
     C                   Unlock    Orcri28k
     C                   ENDIF
     *
 "   c                   endsr
     *****************************************************************
     *    CL_ctlform: controlli formali sul C/L
     *****************************************************************
     C     CL_ctlform    begsr
     *
     * Se documento non valorizzato segnalo l'errore
     C                   if        S1ndoc = *blanks
     C                   eval      $errore = 'E'
     C                   clear                   zbferr
     C                   eval      uborab = S1ubor
     C                   eval      tdorab = S1tdor
     C                   eval      pdorab = S1pdor
     C                   eval      ndorab = S1ndor
     C                   eval      ubptab = S1ubpt
     C                   eval      protab = S1prot
     C                   eval      nrorab = S1nror
     C                   eval      cstgab = aucstg
     C                   eval      cudbab = aucudb
     C                   eval      ccliab = auccli
     C                   eval      nordab = aunord
     C                   eval      nrigab = aunrig
     C                   eval      srigab = ausrig
     C                   eval      noprab = e6nopr
     C                   eval      roprab = e6ropr
     C                   eval      srprab = e6srpr
     C                   eval      ccpaab = $ccpa
     C                   eval      stapab = e6stat
     C                   eval      msgeab = 'SNS8147'
     C                   exsr      Errore
     C                   leavesr
     C                   endif
     * Se documento non valorizzato segnalo l'errore
     C                   if        S1data = *zeros
     C                   eval      $errore = 'E'
     C                   clear                   zbferr
     C                   eval      uborab = S1ubor
     C                   eval      tdorab = S1tdor
     C                   eval      pdorab = S1pdor
     C                   eval      ndorab = S1ndor
     C                   eval      ubptab = S1ubpt
     C                   eval      protab = S1prot
     C                   eval      nrorab = S1nror
     C                   eval      cstgab = aucstg
     C                   eval      cudbab = aucudb
     C                   eval      ccliab = auccli
     C                   eval      nordab = aunord
     C                   eval      nrigab = aunrig
     C                   eval      srigab = ausrig
     C                   eval      noprab = e6nopr
     C                   eval      roprab = e6ropr
     C                   eval      srprab = e6srpr
     C                   eval      ccpaab = $ccpa
     C                   eval      stapab = e6stat
     C                   eval      msgeab = 'SNS8807'
     C                   exsr      Errore
     C                   leavesr
     C                   endif
     *
     * Verificao esistenza in tabella causale variazione  Y1
     c                   eval      $cvar = 'Y1'
     C                   Movel(P)  'VAR'         $ctab
     C                   Movel(P)  $cvar         $cele
     C                   Exsr      Ctl_decotab
     *
     C                   If        @DLI3 = '1'
     C                   eval      $errore = 'E'
     C                   clear                   zbferr
     C                   eval      uborab = S1ubor
     C                   eval      tdorab = S1tdor
     C                   eval      pdorab = S1pdor
     C                   eval      ndorab = S1ndor
     C                   eval      ubptab = S1ubpt
     C                   eval      protab = S1prot
     C                   eval      nrorab = S1nror
     C                   eval      cvarab = $cvar
     C                   eval      msgeab = 'SNS3498'
     C                   exsr      Errore
     C                   leavesr
     *
     C                   Else
     C                   Movel(P)  @dli2         z@cvar
     C                   endif
     *
 "   c                   endsr
     *****************************************************************
     *    SAV_ORC: Salvo ORCRI prima di UPDATE
     *****************************************************************
     C     Sav_orc       begsr
     *
 "   C                   CLEAR                   ZVE500
 "   C                   EVAL      CUDBGF = AUCUDB
 "   C                   EVAL      CSTGGF = AUCSTG
 "   C                   EVAL      CCLIGF = AUCCLI
 "   C                   EVAL      NORDGF = AUNORD
 "   C                   EVAL      NRIGGF = AUNRIG
 "   C                   EVAL      SRIGGF = AUSRIG
*FAOC                   EVAL      CTESGF = AUCTES
FAO02C                   EVAL      CO3TGF = AUCOL3
BET  C                   EVAL      CTECGF = AUCTEC
BET  C                   EVAL      COLCGF = AUCOLC
 "   C                   EVAL      CMODGF = AUCMOD
 "   C                   EVAL      CFPTGF = AUCFPT
 "   C                   EVAL      CFPMGF = AUCFPM
*FAOC                   EVAL      CFODGF = AUCFOD
FAO02C                   MOVE      AUCOLF        CO3FGF
*FAOC                   EVAL      CBOTGF = AUCBOT
FAO02C                   MOVE      AUCOLB        CO3BGF
 "   C                   EVAL      CVFPGF = AUCVFP
 "   C                   EVAL      CIMPGF = AUCIMP
 "   C                   EVAL      TASOGF = AUTASO
 "   C                   EVAL      TSCTGF = AUTSCT
 "   C                   EVAL      CCCTGF = AUCCCT
 "   C                   EVAL      CCIPGF = AUCCIP
 "   C                   EVAL      CSERGF = AUCSER
 "   C                   EVAL      CCMPGF = AUCCMP
 "   C                   EVAL      STATGF = AUSTAT
 "   C                   EVAL      CDROGF = AUCDRO
 "   C                   EVAL      CSTAGF = AUCSTA
 "   C                   EVAL      PSCRGF = AUPSCR
 "   C                   EVAL      VSCRGF = AUVSCR
 "   C                   EVAL      VSREGF = AUVSRE
 "   C                   EVAL      PGMVGF = @PGM
 "   C                   EVAL      GFV01  = AUA01
 "   C                   EVAL      GFV02  = AUA02
 "   C                   EVAL      GFV03  = AUA03
 "   C                   EVAL      GFV04  = AUA04
 "   C                   EVAL      GFV05  = AUA05
 "   C                   EVAL      GFV06  = AUA06
 "   C                   EVAL      GFV07  = AUA07
 "   C                   EVAL      GFV08  = AUA08
 "   C                   EVAL      GFV09  = AUA09
 "   C                   EVAL      GFV10  = AUA10
 "   C                   EVAL      GFV11  = AUA11
 "   C                   EVAL      GFV12  = AUA12
 "   C                   EVAL      GFV13  = AUA13
 "   C                   EVAL      GFV14  = AUA14
 "   C                   EVAL      GFV15  = AUA15
 "   C                   EVAL      GFV16  = AUA16
 "   C                   EVAL      VARIGF = AUCQTA
 "   C                   EVAL      CMOSGF = AUCMOS
*FAOC                   EVAL      TESOGF = AUTESO
FAO02C                   MOVE      AUCOLO        CO3SGF
*FAOC                   EVAL      CTE2GF = AUCTE2
FAO02C                   MOVE      AUCLT3        CO32GF
 "   C                   EVAL      FABRGF = AUFABR
 "   C                   EVAL      NFB2GF = AUNFB2
 "   C                   EVAL      cdstgf = AUcdst
     *
 "   c                   endsr
     *********************************************
     *    Agg_qmag: aggiorno quantit{ a magazzino
     *********************************************
     C     Agg_qmag      begsr
     *
 "   C                   eval      aum01= rlns(1)  + aum01
 "   C                   eval      aum02= rlns(2)  + aum02
 "   C                   eval      aum03= rlns(3)  + aum03
 "   C                   eval      aum04= rlns(4)  + aum04
 "   C                   eval      aum05= rlns(5)  + aum05
 "   C                   eval      aum06= rlns(6)  + aum06
 "   C                   eval      aum07= rlns(7)  + aum07
 "   C                   eval      aum08= rlns(8)  + aum08
 "   C                   eval      aum09= rlns(9)  + aum09
 "   C                   eval      aum10= rlns(10) + aum10
 "   C                   eval      aum11= rlns(11) + aum11
 "   C                   eval      aum12= rlns(12) + aum12
 "   C                   eval      aum13= rlns(13) + aum13
 "   C                   eval      aum14= rlns(14) + aum14
 "   C                   eval      aum15= rlns(15) + aum15
 "   C                   eval      aum16= rlns(16) + aum16
 "   C                   eval      aucqtm = s1toqn + aucqtm
     *
 "   c                   endsr
     *************************************************************
     *    Agg_qmav: aggiorno quantit{ a magazzino virtuali vasca *
     *************************************************************
     C     agg_qtarv     begsr
     *
 "   C                   eval      aucqrv=aucqrv+S1TOQN
 "   C                   eval      aur01=aur01+RLNX(1)
 "   C                   eval      aur02=aur02+RLNX(2)
 "   C                   eval      aur03=aur03+RLNX(3)
 "   C                   eval      aur04=aur04+RLNX(4)
 "   C                   eval      aur05=aur05+RLNX(5)
 "   C                   eval      aur06=aur06+RLNX(6)
 "   C                   eval      aur07=aur07+RLNX(7)
 "   C                   eval      aur08=aur08+RLNX(8)
 "   C                   eval      aur09=aur09+RLNX(9)
 "   C                   eval      aur10=aur10+RLNX(10)
 "   C                   eval      aur11=aur11+RLNX(11)
 "   C                   eval      aur12=aur12+RLNX(12)
 "   C                   eval      aur13=aur13+RLNX(13)
 "   C                   eval      aur14=aur14+RLNX(14)
 "   C                   eval      aur15=aur15+RLNX(15)
 "   C                   eval      aur16=aur16+RLNX(16)
     *
 "   c                   endsr
     *********************************************
     *    Agg_qmag: aggiorno quantit{ a magazzino
     *********************************************
     C     Agg_qmagv     begsr
     *
 "   C                   eval      aum01= rlnx(1)  + aum01
 "   C                   eval      aum02= rlnx(2)  + aum02
 "   C                   eval      aum03= rlnx(3)  + aum03
 "   C                   eval      aum04= rlnx(4)  + aum04
 "   C                   eval      aum05= rlnx(5)  + aum05
 "   C                   eval      aum06= rlnx(6)  + aum06
 "   C                   eval      aum07= rlnx(7)  + aum07
 "   C                   eval      aum08= rlnx(8)  + aum08
 "   C                   eval      aum09= rlnx(9)  + aum09
 "   C                   eval      aum10= rlnx(10) + aum10
 "   C                   eval      aum11= rlnx(11) + aum11
 "   C                   eval      aum12= rlnx(12) + aum12
 "   C                   eval      aum13= rlnx(13) + aum13
 "   C                   eval      aum14= rlnx(14) + aum14
 "   C                   eval      aum15= rlnx(15) + aum15
 "   C                   eval      aum16= rlnx(16) + aum16
 "   C                   eval      aucqtm = s1toqn + aucqtm
     *
 "   c                   endsr
     ******************************************************************
     *    Agg_qta : aggiorno status e date NO BULK
     ******************************************************************
     C     Agg_qta       begsr
     *
     *--Aggiorno quantit{ in essere se non bulk
     C                   if        aucblk = *blanks
     C                   eval      AUa01  = AUm01
     C                   eval      AUa02  = AUm02
     C                   eval      AUa03  = AUm03
     C                   eval      AUa04  = AUm04
     C                   eval      AUa05  = AUm05
     C                   eval      AUa06  = AUm06
     C                   eval      AUa07  = AUm07
     C                   eval      AUa08  = AUm08
     C                   eval      AUa09  = AUm09
     C                   eval      AUa10  = AUm10
     C                   eval      AUa11  = AUm11
     C                   eval      AUa12  = AUm12
     C                   eval      AUa13  = AUm13
     C                   eval      AUa14  = AUm14
     C                   eval      AUa15  = AUm15
     C                   eval      AUa16  = AUm16
     C                   eval      AUCQTA = AUCQTM
     C                   endif
     *
     *--Status e date
     C                   Select
     C                   When      aucqta = 0
     c                   eval      austat = 'AN'
 "   C                   eval      audtan = $$dat8
 "   C                   eval      audtst = $$dat8
     C                   When      austan <= '060'
 "   C                   eval      audtma = $$dat8
 "   C                   eval      audtst = $$dat8
     c                   eval      austat = 'RM'
     C                   endSl
     *
     C     austat        chain     evean01k
     C                   eval      austan = cgstan
     *
 "   c                   endsr
     *****************************************************************
 "   *  RICHIAMO MODULO SCRITTURA ORCVR  se ho scatenato preannullamenti
     *****************************************************************
     C     SAV_ORC2      begsr
 "   C                   EVAL      CVARGF = $CVAR
 "   C                   EVAL      VARIGF = AUCQTA - VARIGF
 "   C                   EVAL      GFV01  = AUA01  - GFV01
 "   C                   EVAL      GFV02  = AUA02  - GFV02
 "   C                   EVAL      GFV03  = AUA03  - GFV03
 "   C                   EVAL      GFV04  = AUA04  - GFV04
 "   C                   EVAL      GFV05  = AUA05  - GFV05
 "   C                   EVAL      GFV06  = AUA06  - GFV06
 "   C                   EVAL      GFV07  = AUA07  - GFV07
 "   C                   EVAL      GFV08  = AUA08  - GFV08
 "   C                   EVAL      GFV09  = AUA09  - GFV09
 "   C                   EVAL      GFV10  = AUA10  - GFV10
 "   C                   EVAL      GFV11  = AUA11  - GFV11
 "   C                   EVAL      GFV12  = AUA12  - GFV12
 "   C                   EVAL      GFV13  = AUA13  - GFV13
 "   C                   EVAL      GFV14  = AUA14  - GFV14
 "   C                   EVAL      GFV15  = AUA15  - GFV15
 "   C                   EVAL      GFV16  = AUA16  - GFV16
 "   C********           MOVEL(P)  ZVE500        KPJBU
 "   C                   CALL      'VE500'
 "   C                   PARM                    KPJBA
 "   C                   PARM                    zve500
     *
 "   c                   endsr
     ******************************************************************
     *    Agg_NO_BLK : gestione conto lavoro in ricezione dati CLD
     *.................................................................
     *    - CL_ctlform       : Controlli formali
     *    - Sav_orc          : Salvataggio dati
     *    - agg_qmag         : Aggiorno q.ta magazzino ORCRI con q.ta ricevuta
     *    - preann_lel       : Gestione annullamenti capi magaz - capi essere
     *    - agg_pro_cl       : Q.ta essere, date, status
     *                         Update + registro variazioni ORCRI
     *    - chi_cl           : Aggiornamento dati produttivi
     *    - chi_cl           : Chiusura conto lavoro
     *    - CAL_LC911        : Aggiornamento file SN4ORD
     *    - ava_pro          : Aggiornamento avanzamenti produttivi
     ******************************************************************
     C     Agg_NO_BLK    begsr
     *
     *--Controlli formali: documento di ingresso e causale di variazione
     C                   exsr      CL_ctlform
     C                   if        $errore <> *blanks
     C                   leavesr
     C                   endif
     *
     *--Aggancio ORCRI, che sicuramente lo trovo, xk} verificato in testata PGM
 "   C     Kriga         Chain     orcri28k
 "   C                   If        not %found
 "   C     Krigap        Chain     orcri28k
 "   C                   EndIf
     *
 "   *--Se la riga } gi{ stata ricevuta, esco senza elaborarla
     C                   select
     C                   when      austan <  '060'
     C                   Unlock    orcri28k
     C                   clear                   zbferr
     C                   eval      uborab = S1ubor
     C                   eval      tdorab = S1tdor
     C                   eval      pdorab = S1pdor
     C                   eval      ndorab = S1ndor
     C                   eval      ubptab = S1ubpt
     C                   eval      protab = S1prot
     C                   eval      nrorab = S1nror
     C                   eval      cstgab = aucstg
     C                   eval      cudbab = aucudb
     C                   eval      ccliab = auccli
     C                   eval      nordab = aunord
     C                   eval      nrigab = aunrig
     C                   eval      srigab = ausrig
     C                   eval      ccpaab = auccpm
     C                   eval      statab = austat
     C                   eval      msgeab = 'SNS8811'
     C                   eval      $errore= 'E'
     C                   exsr      Errore
     C                   leavesr
     C                   when      austan >  '060'
     C                   Unlock    orcri28k
     C                   clear                   zbferr
     C                   eval      uborab = S1ubor
     C                   eval      tdorab = S1tdor
     C                   eval      pdorab = S1pdor
     C                   eval      ndorab = S1ndor
     C                   eval      ubptab = S1ubpt
     C                   eval      protab = S1prot
     C                   eval      nrorab = S1nror
     C                   eval      cstgab = aucstg
     C                   eval      cudbab = aucudb
     C                   eval      ccliab = auccli
     C                   eval      nordab = aunord
     C                   eval      nrigab = aunrig
     C                   eval      srigab = ausrig
     C                   eval      ccpaab = auccpm
     C                   eval      statab = austat
     C                   eval      msgeab = 'SNS8812'
     C                   eval      $errore= 'E'
     C                   exsr      Errore
     C                   leavesr
     C                   endSL
     *
     *--Aggiornamento per ORCRI NO BULK: preannullamenti, quantit{, status, e date
     C                   exsr      preann_lel

     *--Aggancio ORCRI, che sicuramente lo trovo, xk} verificato in testata PGM
 "   C     Kriga         Chain     orcri28k
 "   C                   If        not %found
 "   C     Krigap        Chain     orcri28k
 "   C                   EndIf
     *
 "   *  SALVO DATI RIGA ORDINE
     C                   exsr      Sav_orc
     *
 "   * Aggiorno Q.ta magazzino su ORCRI
     C                   exsr      agg_qmag
     C                   exsr      agg_qta
     *
     C                   exsr      cabfic
     C                   update    rorcri
     *
 "   *--Richiamo modulo scrittura ORCVR  se ho scatenato preannullamenti
     C                   IF        $PRE <> *blanks
     C                   exsr      Sav_orc2
     C                   exsr      Mod_ann_blk
 "   C                   EndIf
     *
 "   *--Aggiornamento di ORPRA x Conto Lavoro
     C                   exsr      agg_pro_CL
     C                   if        $errore <> *blanks
     C                   leavesr
     C                   endif
     *
 "   *--Calcolo ultima fase ciclo/flusso
     C                   exsr      cal_ult_fase
 "   *--Chiusura C/L
     C                   exsr      chi_cl
     C                   if        $errore <> *blanks
     C                   leavesr
     C                   endif
     *
BET  * Richiamo modulo avanzamenti produttivi
     C                   exsr      ava_pro
     *
     * Aggiorno file Web SN4ORD con status SLD
     c                   Exsr      CAL_LC911
     *
     C                   endsr
     *
     ******************************************************************
     *    ContolavoroLE: Sistemazione dati che sono entrati errati
     *.................................................................
     *    - Controlli formali: documento di ingresso valorizzato
     *                         Causale Y1 di capi non trovati esistente
     *    - Agg.to ORCRI     : Salvataggio dati
     *                         Q.ta magazzino
     *                         Gestione annullamenti capi non prodotti
     *                         Q.ta essere, date, status
     *                         Update + registro variazioni
     *    - Agg.to ORPRA     : Allineamento q.ta e status
     *    - C/L              : Chiusura conto lavoro
     *    - Agg. WEB         : Aggiornamento file SN4ORD
     *    - Avanz.Produtt.   : Aggiornamento avanzamenti produttivi
     ******************************************************************
     C     ContolavoroLE begsr
     *
     *--Controlli formali: documento di ingresso e causale di variazione
     C                   exsr      CL_ctlform
     C                   if        $errore <> *blanks
     C                   leavesr
     C                   endif
     *
     *--Aggancio ORCRI, che sicuramente lo trovo, xk} verificato in testata PGM
 "   C     Kriga         Chain     orcri28k
 "   C                   If        not %found
 "   C     Krigap        Chain     orcri28k
 "   C                   EndIf
     *
     *--Se } un ordine BULK non facciamo nulla, in quanto esiste una procedura di Antonella
     *--che fa tutti gli aggiornamenti.
     C**                 If        aucblk <> *blanks
     C**                 eval      $errore = 'S'
     C**                 unlock    orcri28k
     C**                 leavesr
     C**                 endif
     *
 "   *--Se la riga } gi{ stata ricevuta, esco senza elaborarla
     C**                 if        austat <> 'TG'
     C**                 eval      $errore = 'S'
     C**                 unlock    orcri28k
     C**                 leavesr
     C**                 endif
     *
     C**                 If        aucqts <> 0 and aucblk = *BLANKS
     C**                 eval      austat='FA '
     C**                 eval      austaN='230'
     *
     C                   exsr      cabfic
     C                   update    rorcri
     C**                 endif
     *
 "   *--Aggiornamento di ORPRA solo se non } un bulk o il rientro } totale
     C                   if        aucblk = *BLANKS
 "   *--Leggo tutto ORPRA
     C     KorpraSNS     setll     orpra01k
     C                   do        *hival
     C     KorpraSNS     reade     orpra01k
 "   *
     C                   if        %EOF
     C                   leave
     C                   endif
 "   *
     * Aggiorno ORPRA con status SD
     C                   if        e6qlan <> *zeros
     C                   eval      e6stat = 'SD'
     C                   else
     C                   eval      e6stat = 'AN'
     C                   endif
     C     e6stat        chain     evean01k
     C                   eval      e6stan = cgstan
 "   *
     C                   exsr      cabfic
     C                   update    ORPRA01
 "   *
     C                   enddo
     C                   endif
     *
 "   *--Chiusura C/L
     C                   exsr      chi_cl
     C                   if        $errore <> *blanks
     C                   leavesr
     C                   endif
     *
     * Aggiorno file Web SN4ORD con status SLD
BET  C                   If        e6cblk = *Blanks
BET  C                             AND e6qlan<=aucqtm
     c                   Exsr      CAL_LC911
BET  C                   EndIf
     *
BET  * Richiamo modulo avanzamenti produttivi
     C**                 exsr      ava_pro
     *
     C                   endsr
     *
     ************************************************************************
     *    Agg_BLK: gestione conto lavoro in ricezione dati CLD Bulk
     *.......................................................................
     *    - CL_ctlform       : Controlli formali
     *    - agg_qmag         : Aggiorna quantit{ a magazzino orcri
     *    - agg_qta_blk      : Aggiorna quantit{ blk su orcri
     *    - agg_pro_cl       : Aggiornamento produttivo in Conto Lavoro
     *    - Chi_cl           : Aggiornamento/chiusura conto lavoro
     *    - CAL_LC911        : Chiusura WEB file SN4ORD
     *    - ava_pro          : Aggiornamento avanzamenti produttivi
     ******************************************************************
     C     Agg_BLK       begsr
     *
     *--Controlli formali: documento di ingresso e causale di variazione
     C                   exsr      CL_ctlform
     C                   if        $errore <> *blanks
     C                   leavesr
     C                   endif
     *
     *--Aggancio ORCRI, che sicuramente lo trovo, xk} verificato in testata PGM
 "   C     Kriga         Chain     orcri28k
 "   C                   If        not %found
 "   C     Krigap        Chain     orcri28k
 "   C                   EndIf
     *
     *
 "   *--Se la riga } gi{ stata ricevuta, esco senza elaborarla
     C                   select
     C                   when      austan <  '060'
     C                   clear                   zbferr
     C                   eval      uborab = S1ubor
     C                   eval      tdorab = S1tdor
     C                   eval      pdorab = S1pdor
     C                   eval      ndorab = S1ndor
     C                   eval      ubptab = S1ubpt
     C                   eval      protab = S1prot
     C                   eval      nrorab = S1nror
     C                   eval      cstgab = aucstg
     C                   eval      cudbab = aucudb
     C                   eval      ccliab = auccli
     C                   eval      nordab = aunord
     C                   eval      nrigab = aunrig
     C                   eval      srigab = ausrig
     C                   eval      ccpaab = auccpm
     C                   eval      statab = austat
     C                   eval      msgeab = 'SNS8811'
     C                   eval      $errore= 'E'
     C                   exsr      Errore
     C                   unlock    orcri28k
     C                   leavesr
     C                   when      austan >  '060'
     C                   clear                   zbferr
     C                   eval      uborab = S1ubor
     C                   eval      tdorab = S1tdor
     C                   eval      pdorab = S1pdor
     C                   eval      ndorab = S1ndor
     C                   eval      ubptab = S1ubpt
     C                   eval      protab = S1prot
     C                   eval      nrorab = S1nror
     C                   eval      cstgab = aucstg
     C                   eval      cudbab = aucudb
     C                   eval      ccliab = auccli
     C                   eval      nordab = aunord
     C                   eval      nrigab = aunrig
     C                   eval      srigab = ausrig
     C                   eval      ccpaab = auccpm
     C                   eval      statab = austat
     C                   eval      msgeab = 'SNS8812'
     C                   eval      $errore= 'E'
     C                   exsr      Errore
     C                   unlock    orcri28k
     C                   leavesr
     C                   endSL
     *
     C                   exsr      agg_pro_CL
     *
     C                   If        E6stat ='SD' and aucqta <= aucqtm
     C                   Eval      Austat = 'RM'
     C     austat        chain     evean01k
     C                   eval      austan = cgstan
     C                   eval      audtst = $$dat8
     C                   eval      audtma = $$dat8
     C                   Endif
     *
     C                   exsr      cabfic
     C                   update    rorcri
     C                   unlock    orcri28k
     *
 "   *--Calcolo ultima fase ciclo/flusso
     C                   exsr      cal_ult_fase
 "   *--Chiusura C/L
     C                   exsr      chi_cl
     C                   if        $errore <> *blanks
     C                   leavesr
     C                   endif
     *
BET  * Richiamo modulo avanzamenti produttivi
     C                   exsr      ava_pro
     *
     * Aggiorno file Web SN4ORD con status SLD
BET  C                   If        e6stat = 'SD'
     c                   Exsr      CAL_LC911
BET  C                   EndIf
     *
     C                   endsr
     *-----------------------------------------------------------------------------------------
     *    Ricerco su tabella ciclo produttivo
     *-----------------------------------------------------------------------------------------
     C     s_ccpd        begsr
      *
     C                   clear                   z@ccpd
     C                   eval      @dli0 = 'CPD'
     C                   eval      @dli1 = E6CCPD
     C                   call      '$TAB'
     C                   parm                    @dli0             3            Codice
     C                   parm                    @dli1             6            Elemento
     C                   parm                    @dli2           150            Dati della
     C                   parm      *blanks       @dli3             1            Errore '1'
     C                   parm                    lingua            1            Lingua
     C                   parm                    family            1            Famiglia
     C                   movel     @dli2         z@ccpd
     C                   endsr
     ******************************************************************
     *    Calcolo ultima fase ciclo /flusso
     ******************************************************************
     c     cal_ult_fase  begsr
      *
     c                   eval      $tilav=*blank
     c                   eval      $ucfbp=*blank
     c                   eval      $ulfas=*blank
      *
     c     e6ccpd        setgt     cipro02k
     c                   do        *hival
     c     e6ccpd        readpe    cipro02k

     c                   if        %eof(cipro02k)
     c                   leave
     c                   endif
     c
     c                   if        a001h9<>*blank
     c                   iter
     c                   endif
      *
     c     kflufa        setll     flufa01k
     c                   do        *hival
     c     kflufa        reade     flufa01k
     c
     c                   if        %eof(flufa01k)
     c                   leave
     c                   endif
     c
     C                   if        a001ib<>*blank
     c                   iter
     c                   endif
      *
     c                   Clear                   z@cfpd
 "   C                   Movel(P)  'FPD'         $ctab
     c                   if        IBFPDR<>*blank
     c                   movel(p)  ibfpdr        $cele
     c                   else
 "   C                   Movel(P)  ibcfpd        $cele
     c                   endif
     c                   Exsr      Ctl_decotab
     c                   If        @DLI3 <> '1'
     c                   Movel(P)  @dli2         z@cfpd
     c                   else
     c                   iter
     c                   EndIf
      *
      * SE TECNOLOGIA OBBLIGATORIA
     c                   if        ibctcn<>*blanks  and
     c                             ibctcn<>e6ctcn   AND
     c                             FTCNIB= 'Y'
     c                   iter
     c                   endif

      * SE TECNOLOGIA NON OBBLIGATORIA
     c                   if        ibctcn<>*blanks  and
     c                             FTCNIB<>'Y'
     c                   iter
     c                   endif

      * SE LAVORA PER MODELLO PRODUTTIVO
     c                   if        ibccpp<>*blanks  and
     c                             ibccpp<>e6ccpm   and
     c                             FMOPIB= 'Y'
     c                   iter
     c                   endif
      *
     C     Kfape1        chain     fapes01k
     c                   if        %found(fapes01k)
     C                   Eval      $cudf=utudbg
     C                   Eval      $cfor=utcfor
     c                   endif
     c                   eval      $tilav=ctilib
     c                   eval      $ucfbp=ibcfbp
     c                   if        ibfpdr<>*blank
     c                   eval      $ulfas=ibfpdr
     c                   else
     c                   eval      $ulfas=ibcfpd
     c                   endif
     c                   leavesr
      *
     c                   enddo
      *
     c                   enddo
      *
     C                   endsr
     ******************************************************************
     *    Agg_qta_BLK: aggiorno status e date BULK
     ******************************************************************
     C     Agg_qta_BLK   begsr
     *
     *--Status e date: ho q.ta magazzino aggiornato, e q.ta essere vecchio!
     C                   if        aucqtm >= aucqta
 "   C                   eval      audtma = $$dat8
 "   C                   eval      audtst = $$dat8
     c                   eval      austat = 'RM'
     C                   endif
     *
     C     austat        chain     evean01k
     C                   eval      austan = cgstan
     *
     *
     *--Aggiorno quantit{ in essere se non bulk
     C                   if        aua01  < aum01
     C                   eval      AUa01  = AUm01
     C                   eval      AUl01  = AUm01
     C                   endif
     *
     C                   if        aua02  < aum02
     C                   eval      AUa02  = AUm02
     C                   eval      AUl02  = AUm02
     C                   endif
     *
     C                   if        aua03  < aum03
     C                   eval      AUa03  = AUm03
     C                   eval      AUl03  = AUm03
     C                   endif
     *
     C                   if        aua04  < aum04
     C                   eval      AUa04  = AUm04
     C                   eval      AUl04  = AUm04
     C                   endif
     *
     C                   if        aua05  < aum05
     C                   eval      AUa05  = AUm05
     C                   eval      AUl05  = AUm05
     C                   endif
     *
     C                   if        aua06  < aum06
     C                   eval      AUa06  = AUm06
     C                   eval      AUl06  = AUm06
     C                   endif
     *
     C                   if        aua07  < aum07
     C                   eval      AUa07  = AUm07
     C                   eval      AUl07  = AUm07
     C                   endif
     *
     C                   if        aua08  < aum08
     C                   eval      AUa08  = AUm08
     C                   eval      AUl08  = AUm08
     C                   endif
     *
     C                   if        aua09  < aum09
     C                   eval      AUa09  = AUm09
     C                   eval      AUl09  = AUm09
     C                   endif
     *
     C                   if        aua10  < aum10
     C                   eval      AUa10  = AUm10
     C                   eval      AUl10  = AUm10
     C                   endif
     *
     C                   if        aua11  < aum11
     C                   eval      AUa11  = AUm11
     C                   eval      AUl11  = AUm11
     C                   endif
     *
     C                   if        aua12  < aum12
     C                   eval      AUa12  = AUm12
     C                   eval      AUl12  = AUm12
     C                   endif
     *
     C                   if        aua13  < aum13
     C                   eval      AUa13  = AUm13
     C                   eval      AUl13  = AUm13
     C                   endif
     *
     C                   if        aua14  < aum14
     C                   eval      AUa14  = AUm14
     C                   eval      AUl14  = AUm14
     C                   endif
     *
     C                   if        aua15  < aum15
     C                   eval      AUa15  = AUm15
     C                   eval      AUl15  = AUm15
     C                   endif
     *
     C                   if        aua16  < aum16
     C                   eval      AUa16  = AUm16
     C                   eval      AUl16  = AUm16
     C                   endif
     *
     C                   if        aucqta < aucqtm
     C                   eval      AUCQTA = AUCQTM
     C                   eval      AUCQTL = AUCQTM
     C                   endif
     *
 "   c                   endsr
     *
MMD  *****************************************************************
MMD  *    FattuPassiv : Scrivo FPARI(Fatture passive)
MMD  *
MMD  *****************************************************************
MMD  C     FattuPassiv   begsr
MMD  *
MMD  C                   eval      rigaord = S1nror
MMD  *
MMD  * Aggancio Riga Ordine SNS da riferimenti Riga Ordine CSIS
MMD  C     Kriga         chain     orcri28k
MMD  C                   if        not %found
MMD  C     Krigap        chain     orcri28k
MMD  C                   if        not %found
MMD  C                   LeaveSr
MMD  C                   endif
MMD  C                   endif
MMD  *
     C                   if        autsto ='C'
MMD  C                   LeaveSr
     C                   endif
MMD  *
MMD  * Aggancio Riga Ordine di produzione SNS
MMD  C                   eval      e6nopr = aunord
MMD  C                   eval      e6ropr = aunrig
MMD  C                   eval      e6srpr = ausrig
MMD  C                   if        $ccpa <> *blanks
MMD  C     Korpra        setll     orpra55k
MMD  C                   else
MMD  C     KorpraSNS     setll     orpra55k
MMD  C                   endif
MMD   *
MMD  C                   do        *hival
MMD  C                   if        $ccpa <> *blanks
MMD  C     Korpra        reade     orpra55k
MMD  C                   else
MMD  C     KorpraSNS     reade     orpra55k
MMD  C                   endif
MMD  C                   if        %EOF
MMD  C                   leave
MMD  C                   endif
MMD  *
     * Verifico se e' un componente della parte di capo, se si la scarto
     C     e6ccpa        chain     cpman01k
     c                   IF        %found(cpman01k)
     c                             and acfgcm = 'Y'
     c                   iter
     c                   endif
     *
MMD  * Compilo la DS per poter scrivere FPARI
MMD  C                   CLEAR                   zmp810
MMD   *
MMD  C     Kfrnan        chain     frnan01k
MMD  C                   if        %found
MMD  C                   eval      oforcv = b5ofor                              Origine Fornitore
MMD  C                   endif
MMD  C                   eval      cudbcv = ##cudb                              Udb
MMD  C                   eval      udbfcv = e6cudf                              Udb Fornitore
MMD  C                   eval      cforcv = e6cfor                              Fornitore
MMD  C                   eval      csercv = *blanks                             Serie
MMD  C                   eval      ndoccv = *blanks                             Num.Documento
MMD  C                   eval      adoccv = 0                                   Anno Documento
MMD  C                   eval      mdoccv = 0                                   Mese Documento
MMD  C                   eval      gdoccv = 0                                   Giorno Documento
MMD  C                   eval      tbofcv = *blanks                             Bolla/Fattura
MMD   *
MMD   * Forzo momentaneamente la serie a CL per riconoscere che FPARI e' scritto da questo programma
MMD  C                   eval      udoccv = s1ubus                              Udb documento
MMD  C                   eval      csercv = *blanks                             Serie
MMD  C                   eval      ndoccv = s1ndoc                              Num documento
MMD  C                   eval      $$data = s1data                              Data documento
MMD  C                   eval      adoccv = $$anno                              Anno documento
MMD  C                   eval      mdoccv = $$mese                              Mese documento
MMD  C                   eval      gdoccv = $$gior                              Giorno documento
MMD  C                   eval      qmovcv = s1toqn                              Quantita FPARI
MMD  C                   eval      qmovcv = s1toqn                              Quantita FPARI
MMD  C                   eval      tbofcv = '0'                                 Documento non SNS
MMD  C                   eval      dsnscv = 'N'                                 Documento non SNS
MMD   *
MMD   * Valorizzo la DS con valori presi direttamente dalla riga di produzione ORPRA
MMD  C                   eval      cfamcv = e6cfam                              Famiglia
MMD  C                   eval      udbmcv = ##cudb                              Mag.moment a blanks
MMD  C                   eval      cmagcv = 'PREMAG'                            Mag.moment a blanks
MMD  C                   eval      stagcv = e6cstg                              Stagione lancio
MMD  C                   eval      nlancv = e6nlan                              Lancio
NLANC                   eval      NLA4CV = e6nlan                              Lancio
MMD  C                   eval      ngttcv = e6ngtt                              Gruppo di taglio
MMD  C                   eval      ccpmcv = e6ccpm                              Categoria Prod.Modello
MMD  C                   eval      udbpcv = ##cudb                              Udb provenienza
MMD   *
MMD   * I campi riferimenti al movimento di magazzino sono compilati reperendoli dal numeratore ma di fatto
MMD   * non corrispondo ad un vero movimento a magazzino
MMD  C                   clear                   znumer
MMD  C                   movel     'MPFMOV'      numeb0
MMD  C                   call      'NUMER'
MMD  C                   parm                    kpjba
MMD  C                   parm                    znumer
MMD  C                   eval      nmovcv = progb0                              Num.movimento SNS
MMD  C                   eval      rigmcv = 1                                   Riga movimento SNS
     C                   Eval      $$data = $$dat8
MMD  C                   eval      amovcv = $$anno                              Anno movimento SNS
MMD  C                   eval      mmovcv = $$mese                              Mese movimento SNS
MMD  C                   eval      gmovcv = $$gior                              Giorno movimento SNS
MMD   *
MMD  C                   eval      stgocv = e6cstg                              Stagione ordine
MMD  C                   eval      udbocv = e6cudb                              Udb ordine
MMD  C                   eval      cliocv = e6ccli                              Cliente ordine
MMD  C                   eval      norocv = e6nord                              Numero ordine
MMD  C                   eval      nriocv = e6nrig                              Riga ordine
MMD  C                   eval      sriocv = e6srig                              Sottoriga ordine
MMD  C                   eval      cfbpcv = e6cfbp                              Fabbrica produzione
MMD  C                   eval      ctcncv = e6ctcn                              Tecnologia
MMD  C                   eval      cabscv = e6cabs                              Abb.stilistico
MMD  C                   eval      ftricv = *blanks                             Triangolazione
MMD  C                   eval      cmmgcv = 'CCL'                               Causale Movimento Magazzino
MMD  C                   eval      ctescv = e6ctes                              Tessuto
BET  C                   eval      ctescv = e6ctec                              Tessuto commerciale
BET  C                   eval      col3cv = e6colc                              Colore commerciale
BET  C                   eval      tlavcv = sdclav                              Codice Lavorazione
MMD  C                   eval      cmodcv = e6cmod                              Modello
MMD  C                   eval      tliscv = *blanks                             Tipo listino
MMD   *
MMD   * I campi dedicati ai riferimenti della fattura SNS non vengono valorizzati non essendoci
MMD  C                   eval      udfacv = *blanks                             Udb Fattura SNS
MMD  C                   eval      anfacv = *zeros                              Anno Fattura SNS
MMD  C                   eval      sftacv = *blanks                             Serie Fattura SNS
MMD  C                   eval      nfaacv = *zeros                              Numero Fattura SNS
MMD  C                   eval      cmsgcv = *blanks                             Codice messaggio
MMD   *
MMD  C                   movel(p)  zmp810        kpjbu
MMD  C                   call      'MP811'
MMD  C                   parm                    kpjba
MMD
MMD  C                   enddo
MMD   *
MMD  C                   endsr
     ***********************************************************************
     *    Com_BLK      : Rientro capi a magazzino commercializzati - BULK
     ***********************************************************************
     C     Com_BLK       begsr
     *
     *--Controlli formali: documento di ingresso e causale di variazione
     C                   exsr      CL_ctlform
     C                   if        $errore <> *blanks
     C                   leavesr
     C                   endif
     *
     *--Aggancio ORCRI, che sicuramente lo trovo, xk} verificato in testata PGM
 "   C     Kriga         Chain     orcri28k
 "   C                   If        not %found
 "   C     Krigap        Chain     orcri28k
 "   C                   EndIf
     *
 "   *--Se la riga } gi{ stata ricevuta, esco senza elaborarla
     C                   select
     C                   when      austan <  '050'
     C                   clear                   zbferr
     C                   eval      uborab = S1ubor
     C                   eval      tdorab = S1tdor
     C                   eval      pdorab = S1pdor
     C                   eval      ndorab = S1ndor
     C                   eval      ubptab = S1ubpt
     C                   eval      protab = S1prot
     C                   eval      nrorab = S1nror
     C                   eval      cstgab = aucstg
     C                   eval      cudbab = aucudb
     C                   eval      ccliab = auccli
     C                   eval      nordab = aunord
     C                   eval      nrigab = aunrig
     C                   eval      srigab = ausrig
     C                   eval      ccpaab = auccpm
     C                   eval      statab = austat
     C                   eval      msgeab = 'SNS8811'
     C                   eval      $errore= 'E'
     C                   exsr      Errore
     C                   leavesr
     C                   when      austan >  '060'
     C                   clear                   zbferr
     C                   eval      uborab = S1ubor
     C                   eval      tdorab = S1tdor
     C                   eval      pdorab = S1pdor
     C                   eval      ndorab = S1ndor
     C                   eval      ubptab = S1ubpt
     C                   eval      protab = S1prot
     C                   eval      nrorab = S1nror
     C                   eval      cstgab = aucstg
     C                   eval      cudbab = aucudb
     C                   eval      ccliab = auccli
     C                   eval      nordab = aunord
     C                   eval      nrigab = aunrig
     C                   eval      srigab = ausrig
     C                   eval      ccpaab = auccpm
     C                   eval      statab = austat
     C                   eval      msgeab = 'SNS8812'
     C                   eval      $errore= 'S'
     C                   exsr      Errore
     C                   leavesr
     C                   endSL
     *
 "   *--Aggiornamento di ORPRA solo se non } un bulk
     C                   exsr      agg_orpra
     C                   if        $errore <> *blanks
     C                   leavesr
     C                   endif
     *
 "   * Aggiorno Q.ta magazzino su ORCRI
     *
     C                   if        aucqtm >= aucqta and e6stat='SD'
 "   C                   eval      audtst = $$dat8
 "   C                   eval      audtma = $$dat8
     c                   eval      austat = 'RM'
     C     austat        chain     evean01k
     C                   eval      austan = cgstan
     C                   exsr      cabfic
     C                   update    rorcri
     C                   else
     C                   unlock    orcri28k
     C*                  eval      $iter = 'Y'
     C                   endif
     *
     * Aggiorno file Web SN4ORD con status SLD
BET  C                   If        e6stat = 'SD'
     c                   Exsr      CAL_LC911
BET  C                   EndIf
     *
BET  * Richiamo modulo avanzamenti produttivi
     C*                  exsr      ava_pro
     *
     C                   endsr
     *
     *********************************************************************
     *    Com_no_BLK    : Rientro capi a magazzino commercializzati - NO BULK
     *********************************************************************
     C     Com_no_BLK    begsr
     *
     *--Controlli formali: documento di ingresso e causale di variazione
     C                   exsr      CL_ctlform
     C                   if        $errore <> *blanks
     C                   leavesr
     C                   endif
     *
     *--Aggancio ORCRI, che sicuramente lo trovo, xk} verificato in testata PGM
 "   C     Kriga         Chain     orcri28k
 "   C                   If        not %found
 "   C     Krigap        Chain     orcri28k
 "   C                   EndIf
     *
 "   *--Se la riga } gi{ stata ricevuta, esco senza elaborarla
     C                   select
     C                   when      austan <  '060'
     C                   UNlock    Orcri28k
     C                   clear                   zbferr
     C                   eval      uborab = S1ubor
     C                   eval      tdorab = S1tdor
     C                   eval      pdorab = S1pdor
     C                   eval      ndorab = S1ndor
     C                   eval      ubptab = S1ubpt
     C                   eval      protab = S1prot
     C                   eval      nrorab = S1nror
     C                   eval      cstgab = aucstg
     C                   eval      cudbab = aucudb
     C                   eval      ccliab = auccli
     C                   eval      nordab = aunord
     C                   eval      nrigab = aunrig
     C                   eval      srigab = ausrig
     C                   eval      ccpaab = auccpm
     C                   eval      statab = austat
     C                   eval      msgeab = 'SNS8811'
     C                   eval      $errore= 'E'
     C                   exsr      Errore
     C                   leavesr
     C                   when      austan >  '060'
     C                   UNlock    Orcri28k
     C                   clear                   zbferr
     C                   eval      uborab = S1ubor
     C                   eval      tdorab = S1tdor
     C                   eval      pdorab = S1pdor
     C                   eval      ndorab = S1ndor
     C                   eval      ubptab = S1ubpt
     C                   eval      protab = S1prot
     C                   eval      nrorab = S1nror
     C                   eval      cstgab = aucstg
     C                   eval      cudbab = aucudb
     C                   eval      ccliab = auccli
     C                   eval      nordab = aunord
     C                   eval      nrigab = aunrig
     C                   eval      srigab = ausrig
     C                   eval      ccpaab = auccpm
     C                   eval      statab = austat
     C                   eval      msgeab = 'SNS8812'
     C                   eval      $errore= 'E'
     C                   exsr      Errore
     C                   eval      $iter='Y'
     C                   leavesr
     C                   endSL
     *
     *--Aggiornamento per ORCRI NO BULK: preannullamenti, quantit{, status, e date
     C                   exsr      preann_lel
     *
     *--Aggancio ORCRI, che sicuramente lo trovo, xk} verificato in testata PGM
 "   C     Kriga         Chain     orcri28k
 "   C                   If        not %found
 "   C     Krigap        Chain     orcri28k
 "   C                   EndIf
     *
 "   *  SALVO DATI RIGA ORDINE
     C                   exsr      Sav_orc
     *
 "   * Aggiorno Q.ta magazzino su ORCRI
     C                   exsr      agg_qmag
     C                   exsr      agg_qta
     *
     C                   exsr      cabfic
     C                   update    rorcri
     *
 "   *--Richiamo modulo scrittura ORCVR  se ho scatenato preannullamenti
     C                   IF        $PRE <> *blanks
     C                   exsr      Sav_orc2
     C                   exsr      Mod_ann_blk
 "   C                   EndIf
     *
 "   *--Aggiornamento di ORPRA solo se non } un bulk
     C                   exsr      agg_orpra
     C                   if        $errore <> *blanks
     C                   leavesr
     C                   endif
     *
     * Aggiorno file Web SN4ORD con status SLD
     c                   Exsr      CAL_LC911
     *
BET  * Richiamo modulo avanzamenti produttivi
     C*                  exsr      ava_pro
     *
     C                   endsr
     *
BET1 *---------------------------------------------------------------*
 "   * Alimentazione facon WEB - Estrazione Ordini -                 *
 "   *---------------------------------------------------------------*
 "   C     CAL_LC911     BEGSR
 "   *
 "   C                   clear                   zlc911
 "   C                   eval      $xcstgab = e6cstg
 "   C                   eval      $xcudbab = e6cudb
 "   C                   eval      $xccliab = e6ccli
 "   C                   eval      $xnordab = e6nord
 "   C                   eval      $xnrigab = e6nrig
 "   C                   eval      $xsrigab = e6srig
 "   C                   eval      $xnoprab = e6nopr
 "   C                   eval      $xroprab = e6ropr
 "   C                   eval      $xsrprab = e6srpr
 "   C                   eval      $xccppab = e6ccpa
 "   c                   eval      $xstatab = 'SLD'
 "    *
 "   c                   movel     zlc911        kpjbu
 "   c                   call      'LC911'
 "   c                   parm                    kpjba
 "   *
 "   c                   ENDSR
BET  *****************************************************************
 "   *    Verifica_prea : Richiamo modulo che verifica il preannullamento
 "   *    mi scrive gli archivi ORCPA o ORCPP e controlla le quantita
 "   *****************************************************************
 "   C     Allinea_fasi  BegSr
 "   *
 "   * Se capo accoppiato scrivo solo una volta l'ORCPA
 "   c                   If        e6ccpa <> e6ccpp  AND
 "   c                             e6ccpa <> *Blanks
 "   c                             and $cont > 1
 "   c                   LeaveSr
 "   c                   EndIf
 "   * Recupero fase pi` elevata con quantita in essere
 "   C                   Clear                   zsn679
 "   C                   Eval      $1FAREI1 = 'N'
 "   C                   Eval      $1TEQEI1 = 'Y'
 "   C                   Eval      $1CUDBI1 = e6cudb
 "   C                   Eval      $1CCLII1 = e6ccli
 "   C                   Eval      $1CSTGI1 = e6cstg
 "   C                   Eval      $1NORDI1 = e6nord
 "   C                   Eval      $1NRIGI1 = e6nrig
 "   C                   Eval      $1SRIGI1 = e6srig
 "   C                   Eval      $1NOPRI1 = e6nopr
 "   C                   Eval      $1ROPRI1 = e6ropr
 "   C                   Eval      $1SRPRI1 = e6srpr
 "   C                   Eval      $1CCPTI1 = e6ccpM
 "   C                   Eval      $1CCPCI1 = e6ccpp
 "   C                   Eval      $1CCPAI1 = e6ccpa
 "   C                   Eval      $kpjbu   = kpjbu
 "   C                   Eval      kpjbu    = zsn679
 "
 "   C                   Call      'SN679'
 "   C                   Parm                    kpjba
 "
 "   C                   Eval      zsn679   = kpjbu
 "   C                   Eval      kpjbu    = $kpjbu
 "
 "   c                   If        $1criti1 <> *blanks
 "   c                             OR $1cfpai1 = *Blanks
 "   c                   LeaveSr
 "   c                   EndIf
 "   * Allineo fase pi` elevata con quantita in essere
 "   C                   Clear                   zsn680
 "   C                   Eval      $_pgmci2 = 'BF003L'
 "   C                   Eval      $_FICOI2 = 'Y'
 "   C                   Eval      $_GECOI2 = 'N'
 "   C                   Eval      $_CUDBI2 = e6cudb
 "   C                   Eval      $_CCLII2 = e6ccli
 "   C                   Eval      $_CSTGI2 = e6cstg
 "   C                   Eval      $_NORDI2 = e6nord
 "   C                   Eval      $_NRIGI2 = e6nrig
 "   C                   Eval      $_SRIGI2 = e6srig
 "   C                   Eval      $_NOPRI2 = e6nopr
 "   C                   Eval      $_ROPRI2 = e6ropr
 "   C                   Eval      $_SRPRI2 = e6srpr
 "
 "   C                   Eval      $_QTATI2 = qtaafy
 "   C                   Eval      $_QTA01  = afy01
 "   C                   Eval      $_QTA02  = afy02
 "   C                   Eval      $_QTA03  = afy03
 "   C                   Eval      $_QTA04  = afy04
 "   C                   Eval      $_QTA05  = afy05
 "   C                   Eval      $_QTA06  = afy06
 "   C                   Eval      $_QTA07  = afy07
 "   C                   Eval      $_QTA08  = afy08
 "   C                   Eval      $_QTA09  = afy09
 "   C                   Eval      $_QTA10  = afy10
 "   C                   Eval      $_QTA11  = afy11
 "   C                   Eval      $_QTA12  = afy12
 "   C                   Eval      $_QTA13  = afy13
 "   C                   Eval      $_QTA14  = afy14
 "   C                   Eval      $_QTA15  = afy15
 "   C                   Eval      $_QTA16  = afy16
 "
 "   *  Annullo fase
 "   C                   Eval      $_CCPPI2 = e6ccpp
 "   C                   Eval      $_CCP1I2 = e6ccpa
 "   C                   Eval      $_UDB1I2 = $1UDBAI1
 "   C                   Eval      $_CFB1I2 = $1CFBAI1
 "   C                   Eval      $_CFP1I2 = $1CFPAI1
 "   C                   Eval      $_MAS1I2 = $1MASMI1
 "   C                   Eval      $_MAA1I2 = $1MAAMI1
 "   C                   Eval      $_TOP1I2 = 'ANN'
 "   C                   Eval      $kpjbu   = kpjbu
 "   C                   Eval      kpjbu    = zsn680
 "
 "   C                   Call      'SN680'
 "   C                   Parm                    kpjba
 "
 "   C                   Eval      zsn680   = kpjbu
 "   C                   Eval      kpjbu    = $kpjbu
 "
 "   C                   If        $_criti2 = 'ER'
 "   C*                  eval      $errore = 'E'
 "   C                   clear                   zbferr
 "   C                   eval      cstgab = aucstg
 "   C                   eval      cudbab = aucudb
 "   C                   eval      ccliab = auccli
 "   C                   eval      nordab = aunord
 "   C                   eval      nrigab = aunrig
 "   C                   eval      srigab = ausrig
 "   C                   eval      ccpaab = auccpm
 "   C                   eval      msgeab = $_cmsgi2
 "   C*                  exsr      Errore
 "   C*                  leave
 "   C                   EndIf
 "   *
BET  c                   EndSr
BET  *****************************************************************
 "   *    RiprAvprdMag  : Ripristina quantita ricevute su fase magazzino
 "   *****************************************************************
 "   C     RiprAvprdMag  BegSr
 "   *
 "   C                   Clear                   zsn680
 "   C                   Eval      $_pgmci2 = 'BF003L'
 "   C                   Eval      $_FICOI2 = 'Y'
 "   C                   Eval      $_GECOI2 = 'N'
 "   C                   Eval      $_CUDBI2 = e6cudb
 "   C                   Eval      $_CCLII2 = e6ccli
 "   C                   Eval      $_CSTGI2 = e6cstg
 "   C                   Eval      $_NORDI2 = e6nord
 "   C                   Eval      $_NRIGI2 = e6nrig
 "   C                   Eval      $_SRIGI2 = e6srig
 "   C                   Eval      $_NOPRI2 = e6nopr
 "   C                   Eval      $_ROPRI2 = e6ropr
 "   C                   Eval      $_SRPRI2 = e6srpr
 "
 "   C                   Eval      $_QTATI2 = aucqtm
 "   C                   Eval      $_QTA01  = aum01
 "   C                   Eval      $_QTA02  = aum02
 "   C                   Eval      $_QTA03  = aum03
 "   C                   Eval      $_QTA04  = aum04
 "   C                   Eval      $_QTA05  = aum05
 "   C                   Eval      $_QTA06  = aum06
 "   C                   Eval      $_QTA07  = aum07
 "   C                   Eval      $_QTA08  = aum08
 "   C                   Eval      $_QTA09  = aum09
 "   C                   Eval      $_QTA10  = aum10
 "   C                   Eval      $_QTA11  = aum11
 "   C                   Eval      $_QTA12  = aum12
 "   C                   Eval      $_QTA13  = aum13
 "   C                   Eval      $_QTA14  = aum14
 "   C                   Eval      $_QTA15  = aum15
 "   C                   Eval      $_QTA16  = aum16
 "
 "   *  Annullo fase
 "   C                   Eval      $_CCPPI2 = e6ccpp
 "   C                   Eval      $_CCP1I2 = auccpm
 "   C                   Eval      $_UDB1I2 = ##cudb
 "   C                   Eval      $_CFP1I2 = h5fmag
 "   C                   Eval      $_TOP1I2 = 'RIP'
 "   C                   Eval      $kpjbu   = kpjbu
 "   C                   Eval      kpjbu    = zsn680
 "
 "   C                   Call      'SN680'
 "   C                   Parm                    kpjba
 "
 "   C                   Eval      zsn680   = kpjbu
 "   C                   Eval      kpjbu    = $kpjbu
 "
 "   C                   If        $_criti2 = 'ER'
 "   C*                  eval      $errore = 'E'
 "   C                   clear                   zbferr
 "   C                   eval      cstgab = aucstg
 "   C                   eval      cudbab = aucudb
 "   C                   eval      ccliab = auccli
 "   C                   eval      nordab = aunord
 "   C                   eval      nrigab = aunrig
 "   C                   eval      srigab = ausrig
 "   C                   eval      ccpaab = auccpm
 "   C                   eval      msgeab = $_cmsgi2
 "   C*                  exsr      Errore
 "   C*                  leave
 "   C                   EndIf
 "   *
 "   c                   ENDSR
BET  *****************************************************************
 "   *    RiprMagazzino : Ricarica  quantita essere su fase magazzino
 "   *****************************************************************
 "   C     RicaAvprdMag  BegSr
 "   *
 "   C                   Clear                   zsn680
 "   C                   Eval      $_pgmci2 = 'BF003L'
 "   C                   Eval      $_FICOI2 = 'Y'
 "   C                   Eval      $_GECOI2 = 'N'
 "   C                   Eval      $_CUDBI2 = e6cudb
 "   C                   Eval      $_CCLII2 = e6ccli
 "   C                   Eval      $_CSTGI2 = e6cstg
 "   C                   Eval      $_NORDI2 = e6nord
 "   C                   Eval      $_NRIGI2 = e6nrig
 "   C                   Eval      $_SRIGI2 = e6srig
 "   C                   Eval      $_NOPRI2 = e6nopr
 "   C                   Eval      $_ROPRI2 = e6ropr
 "   C                   Eval      $_SRPRI2 = e6srpr
 "
 "   C                   Eval      $_QTATI2 = aucqta
 "   C                   Eval      $_QTA01  = aua01
 "   C                   Eval      $_QTA02  = aua02
 "   C                   Eval      $_QTA03  = aua03
 "   C                   Eval      $_QTA04  = aua04
 "   C                   Eval      $_QTA05  = aua05
 "   C                   Eval      $_QTA06  = aua06
 "   C                   Eval      $_QTA07  = aua07
 "   C                   Eval      $_QTA08  = aua08
 "   C                   Eval      $_QTA09  = aua09
 "   C                   Eval      $_QTA10  = aua10
 "   C                   Eval      $_QTA11  = aua11
 "   C                   Eval      $_QTA12  = aua12
 "   C                   Eval      $_QTA13  = aua13
 "   C                   Eval      $_QTA14  = aua14
 "   C                   Eval      $_QTA15  = aua15
 "   C                   Eval      $_QTA16  = aua16
 "
 "   *  Annullo fase
 "   C                   Eval      $_CCPPI2 = e6ccpp
 "   C                   Eval      $_CCP1I2 = auccpm
 "   C                   Eval      $_UDB1I2 = ##cudb
 "   C                   Eval      $_CFP1I2 = h5fmag
 "   C                   Eval      $_TOP1I2 = 'SCR'
 "   C                   Eval      $_CCP2I2 = auccpm
 "   C                   Eval      $_CPA2I2 = e6ccpa                            **LEL**
 "   C                   Eval      $_UDB2I2 = ##cudb
 "   C                   Eval      $_CFP2I2 = h5fmag
 "   C                   Eval      $_TOP2I2 = 'CAR'
 "   C                   Eval      $kpjbu   = kpjbu
 "   C                   Eval      kpjbu    = zsn680
 "
 "   C                   Call      'SN680'
 "   C                   Parm                    kpjba
 "
 "   C                   Eval      zsn680   = kpjbu
 "   C                   Eval      kpjbu    = $kpjbu
 "
 "   C                   If        $_criti2 = 'ER'
 "   C*                  eval      $errore = 'E'
 "   C                   clear                   zbferr
 "   C                   eval      cstgab = aucstg
 "   C                   eval      cudbab = aucudb
 "   C                   eval      ccliab = auccli
 "   C                   eval      nordab = aunord
 "   C                   eval      nrigab = aunrig
 "   C                   eval      srigab = ausrig
 "   C                   eval      ccpaab = auccpm
 "   C                   eval      msgeab = $_cmsgi2
 "   C*                  exsr      Errore
 "   C*                  leave
 "   C                   EndIf
 "   *
 "   c                   ENDSR
BET  *****************************************************************
 "   *    Chiudi_AVPRD  : Richiamo modulo che verifica la fase reale
 "   *    piu elevata e la salda
 "   *****************************************************************
 "   C     Chiudi_AVPRD  BegSr
 "   *
 "   * Se capo accoppiato scrivo solo una volta l'ORCPA
 "   c                   If        e6ccpa <> e6ccpp  AND
 "   c                             e6ccpa <> *Blanks
 "   c                             and $cont1 > 1
 "   c                   LeaveSr
 "   c                   EndIf
 "   C                   Eval      $cont1   = $cont1 + 1
 "   * Recupero fase pi` elevata con quantita in essere
 "   C                   Clear                   zsn679
 "   C                   Eval      $1FAREI1 = 'Y'
 "   C                   Eval      $1TEQEI1 = 'Y'
 "   C                   Eval      $1CUDBI1 = e6cudb
 "   C                   Eval      $1CCLII1 = e6ccli
 "   C                   Eval      $1CSTGI1 = e6cstg
 "   C                   Eval      $1NORDI1 = e6nord
 "   C                   Eval      $1NRIGI1 = e6nrig
 "   C                   Eval      $1SRIGI1 = e6srig
 "   C                   Eval      $1NOPRI1 = e6nopr
 "   C                   Eval      $1ROPRI1 = e6ropr
 "   C                   Eval      $1SRPRI1 = e6srpr
 "   C                   Eval      $1CCPTI1 = e6ccpM
 "   C                   Eval      $1CCPCI1 = e6ccpp
 "   C                   Eval      $1CCPAI1 = e6ccpa
 "   C                   Eval      $kpjbu   = kpjbu
 "   C                   Eval      kpjbu    = zsn679
 "
 "   C                   Call      'SN679'
 "   C                   Parm                    kpjba
 "
 "   C                   Eval      zsn679   = kpjbu
 "   C                   Eval      kpjbu    = $kpjbu
 "
 "   c                   If        $1criti1 <> *blanks
 "   c                             OR $1cfpai1 = *Blanks
 "   c                   LeaveSr
 "   c                   EndIf
 "   * Allineo fase pi` elevata con quantita in essere
 "   C                   Clear                   zsn680
 "   C                   Eval      $_pgmci2 = 'BF003L'
 "   C                   Eval      $_FICOI2 = 'Y'
 "   C                   Eval      $_GECOI2 = 'N'
 "   C                   Eval      $_CUDBI2 = e6cudb
 "   C                   Eval      $_CCLII2 = e6ccli
 "   C                   Eval      $_CSTGI2 = e6cstg
 "   C                   Eval      $_NORDI2 = e6nord
 "   C                   Eval      $_NRIGI2 = e6nrig
 "   C                   Eval      $_SRIGI2 = e6srig
 "   C                   Eval      $_NOPRI2 = e6nopr
 "   C                   Eval      $_ROPRI2 = e6ropr
 "   C                   Eval      $_SRPRI2 = e6srpr
 "
 "   *  Annullo fase se ho quantita annullate
 "   C                   If        qtaafy <> *Zeros
 "   C                   Eval      $_QTATI2 = qtaafy
 "   C                   Eval      $_QTA01  = afy01
 "   C                   Eval      $_QTA02  = afy02
 "   C                   Eval      $_QTA03  = afy03
 "   C                   Eval      $_QTA04  = afy04
 "   C                   Eval      $_QTA05  = afy05
 "   C                   Eval      $_QTA06  = afy06
 "   C                   Eval      $_QTA07  = afy07
 "   C                   Eval      $_QTA08  = afy08
 "   C                   Eval      $_QTA09  = afy09
 "   C                   Eval      $_QTA10  = afy10
 "   C                   Eval      $_QTA11  = afy11
 "   C                   Eval      $_QTA12  = afy12
 "   C                   Eval      $_QTA13  = afy13
 "   C                   Eval      $_QTA14  = afy14
 "   C                   Eval      $_QTA15  = afy15
 "   C                   Eval      $_QTA16  = afy16
 "
 "   C                   Eval      $_CCPPI2 = e6ccpp
 "   C                   Eval      $_CCP1I2 = $1CCPAI1
 "   C                   Eval      $_UDB1I2 = $1UDBAI1
 "   C                   Eval      $_CFB1I2 = $1CFBAI1
 "   C                   Eval      $_CFP1I2 = $1CFPAI1
 "   C                   Eval      $_MAS1I2 = $1MASMI1
 "   C                   Eval      $_MAA1I2 = $1MAAMI1
 "   C                   Eval      $_TOP1I2 = 'ANN'
 "   C                   Eval      $kpjbu   = kpjbu
 "   C                   Eval      kpjbu    = zsn680
 "
 "   C                   Call      'SN680'
 "   C                   Parm                    kpjba
 "
 "   C                   Eval      zsn680   = kpjbu
 "   C                   Eval      kpjbu    = $kpjbu
 "
 "   C                   If        $_criti2 = 'ER'
 "   C*                  eval      $errore = 'E'
 "   C                   clear                   zbferr
 "   C                   eval      cstgab = aucstg
 "   C                   eval      cudbab = aucudb
 "   C                   eval      ccliab = auccli
 "   C                   eval      nordab = aunord
 "   C                   eval      nrigab = aunrig
 "   C                   eval      srigab = ausrig
 "   C                   eval      ccpaab = auccpm
 "   C                   eval      msgeab = $_cmsgi2
 "   C*                  exsr      Errore
 "   C*                  leave
 "   C                   EndIf
 "   C                   EndIf
 "
 "   *  Allineo fase se ho quantita residue
 "   C                   If        e6qlan <> *Zeros
 "   C                   Eval      $_QTATI2 = e6qlan
 "   C                   Eval      $_QTA01  = e6q01
 "   C                   Eval      $_QTA02  = e6q02
 "   C                   Eval      $_QTA03  = e6q03
 "   C                   Eval      $_QTA04  = e6q04
 "   C                   Eval      $_QTA05  = e6q05
 "   C                   Eval      $_QTA06  = e6q06
 "   C                   Eval      $_QTA07  = e6q07
 "   C                   Eval      $_QTA08  = e6q08
 "   C                   Eval      $_QTA09  = e6q09
 "   C                   Eval      $_QTA10  = e6q10
 "   C                   Eval      $_QTA11  = e6q11
 "   C                   Eval      $_QTA12  = e6q12
 "   C                   Eval      $_QTA13  = e6q13
 "   C                   Eval      $_QTA14  = e6q14
 "   C                   Eval      $_QTA15  = e6q15
 "   C                   Eval      $_QTA16  = e6q16
 "
 "   C                   Eval      $_CCPPI2 = e6ccpp
 "   C                   Eval      $_CCP1I2 = $1CCPAI1
 "   C                   Eval      $_UDB1I2 = $1UDBAI1
 "   C                   Eval      $_CFB1I2 = $1CFBAI1
 "   C                   Eval      $_CFP1I2 = $1CFPAI1
 "   C                   Eval      $_MAS1I2 = $1MASMI1
 "   C                   Eval      $_MAA1I2 = $1MAAMI1
 "   C                   Eval      $_TOP1I2 = 'SCA'
 "
 "   C                   Eval      $_CCP2I2 = $1CCPAI1
 "   C                   Eval      $_UDB2I2 = $1UDBAI1
 "   C                   Eval      $_CFB2I2 = $1CFBAI1
 "   C                   Eval      $_CFP2I2 = $1CFPAI1
 "   C                   Eval      $_MAS2I2 = $1MASMI1
 "   C                   Eval      $_MAA2I2 = $1MAAMI1
 "   C                   Eval      $_TOP2I2 = 'SCR'
 "
 "   C                   Eval      $kpjbu   = kpjbu
 "   C                   Eval      kpjbu    = zsn680
 "
 "   C                   Call      'SN680'
 "   C                   Parm                    kpjba
 "
 "   C                   Eval      zsn680   = kpjbu
 "   C                   Eval      kpjbu    = $kpjbu
 "
 "   C                   If        $_criti2 = 'ER'
 "   C*                  eval      $errore = 'E'
 "   C                   clear                   zbferr
 "   C                   eval      cstgab = aucstg
 "   C                   eval      cudbab = aucudb
 "   C                   eval      ccliab = auccli
 "   C                   eval      nordab = aunord
 "   C                   eval      nrigab = aunrig
 "   C                   eval      srigab = ausrig
 "   C                   eval      ccpaab = auccpm
 "   C                   eval      msgeab = $_cmsgi2
 "   C*                  exsr      Errore
 "   C*                  leave
 "   C                   EndIf
 "   C                   EndIf
 "   *
BET  c                   EndSr
BET  ************************************************
 "   *    Agg_orpra : Aggiornamento ordine produttivo
 "   ************************************************
 "   C     Agg_orpra     BegSr
 "   *
BET  C*                  eval      $errore = ' '
 "   *
 "   *--Leggo tutto ORPRA
     C     KorpraSNS     setll     orpra01k
     C                   do        *hival
     C     KorpraSNS     reade     orpra01k
 "   *
     C                   if        %EOF
     C                   leave
     C                   endif
 "   *
     * Verifico se e' un componente della parte di capo, se si la scarto
     C     e6ccpa        chain     cpman01k
     c                   IF        %found(cpman01k)
     c                             and acfgcm = 'Y'
     c                   iter
     c                   endif
     *
     * Per definizione lo status deve essere in WP o LP
     C                   if        e6stan < '050'
     C                             and e6stan > '150'
     C                   eval      $errore = 'E'
     C                   clear                   zbferr
     C                   eval      uborab = S1ubor
     C                   eval      tdorab = S1tdor
     C                   eval      pdorab = S1pdor
     C                   eval      ndorab = S1ndor
     C                   eval      ubptab = S1ubpt
     C                   eval      protab = S1prot
     C                   eval      nrorab = S1nror
     C                   eval      cstgab = aucstg
     C                   eval      cudbab = aucudb
     C                   eval      ccliab = auccli
     C                   eval      nordab = aunord
     C                   eval      nrigab = aunrig
     C                   eval      srigab = ausrig
     C                   eval      noprab = e6nopr
     C                   eval      roprab = e6ropr
     C                   eval      srprab = e6srpr
     C                   eval      ccpaab = $ccpa
     C                   eval      stapab = e6stat
     C                   eval      msgeab = 'SNS8813'
     C                   exsr      Errore
     C                   iter
     C                   endif
     *
     * Se tutto va bene Aggiorno quantit{ di ORPRA
     C                   eval      e6qlan = aucqta
     C                   eval      e6q01  = aua01
     C                   eval      e6q02  = aua02
     C                   eval      e6q03  = aua03
     C                   eval      e6q04  = aua04
     C                   eval      e6q05  = aua05
     C                   eval      e6q06  = aua06
     C                   eval      e6q07  = aua07
     C                   eval      e6q08  = aua08
     C                   eval      e6q09  = aua09
     C                   eval      e6q10  = aua10
     C                   eval      e6q11  = aua11
     C                   eval      e6q12  = aua12
     C                   eval      e6q13  = aua13
     C                   eval      e6q14  = aua14
     C                   eval      e6q15  = aua15
     C                   eval      e6q16  = aua16
     *
     * Aggiorno ORPRA con status SD: NO BLK
     C                   if        e6cblk = *blanks and  $assegna<>'1'
     C                   if        e6qlan <> *zeros
     C                   eval      e6stat = 'SD'
     C                   else
     C                   eval      e6stat = 'AN'
     C                   endif
     C                   endif
PG1  * ------------------------------------------------
 "   * solo se versato uguale a essere (totale versato)
 "   c                   if        $assegna='1' and
 "   c                             AUCQTM = AUCQTA
 "    *
 "   c                   if        e6qlan <> *zeros
 "   c                   eval      e6stat = 'SD'
 "   c                   else
 "   c                   eval      e6stat = 'AN'
 "   c                   endif
 "    *
 "   c                   Endif
PG1  * ------------------------------------------------
     *
     * Aggiorno ORPRA con status: BLK
     C                   if        e6cblk <> *blanks and $assegna='0'
     C                   select
     C                   when      e6qlan = 0
     C                   eval      e6stat = 'AN'
     C                   when      autsto = 'C' and aucqtm >= aucqta
     C                   Eval      E6stat = 'SD'
     C                   when      ifnewa = 'Y'
     C                   Exsr      Ctl_Avan
     C  N75              eval      e6stat = 'SD'
     C                   when      ifnewa <> 'Y'
     C                   Clear                   zbf042
     C                   Exsr      Sal_ORPRA
     C                   If        FSALBU = 'Y'
     C                   eval      e6stat = 'SD'
     C                   Endif
     C                   endSL
     C                   endif
     *
     C     e6stat        chain     evean01k
     C                   eval      e6stan = cgstan
 "   *
     C                   exsr      cabfic
     C                   update    ORPRA01
 "   *
     C                   enddo
 "   *
BET  c                   EndSr
     *-----------------------------------------------------------------*
     *- CTL_Avan - Controlli sugli avanzamenti                         *
     *-----------------------------------------------------------------*
     C     Ctl_Avan      begsr
     *
     C                   SETOFF                                       75
     C     Korcpa        Setll     Avprd03k
     C                   Do        *Hival
     C     Korcpa        Reade     Avprd03k
     *
     C                   If        %Eof(Avprd03k)
     C                   Leave
     C                   EndIf
     *
     C                   If        $ICQRIE <> 0 or $ICQESS <> 0
     C                   SetOn                                        75
     C                   Leave
     C                   EndIf
     *
     C                   EndDo
     *
     C                   endsr
BET  ************************************************
 "   *    agg_pro_CL: Aggiornamento ORPRA in C/L
 "   ************************************************
 "   C     Agg_pro_CL    BegSr
 "   *
BET  C*                  eval      $errore = ' '
 "   *
 "   *--Leggo tutto ORPRA
     C     KorpraSNS     setll     orpra01k
     C                   do        *hival
     C     KorpraSNS     reade     orpra01k
 "   *
     C                   if        %EOF
     C                   leave
     C                   endif
 "   *
     * Verifico se e' un componente della parte di capo, se si la scarto
     C     e6ccpa        chain     cpman01k
     c                   IF        %found(cpman01k)
     c                             and acfgcm = 'Y'
     c                   iter
     c                   endif
     *
     * Per definizione lo status deve essere in AC!
     C                   if        e6stat <>'AC '
     C                   eval      $errore = 'E'
     C                   clear                   zbferr
     C                   eval      uborab = S1ubor
     C                   eval      tdorab = S1tdor
     C                   eval      pdorab = S1pdor
     C                   eval      ndorab = S1ndor
     C                   eval      ubptab = S1ubpt
     C                   eval      protab = S1prot
     C                   eval      nrorab = S1nror
     C                   eval      cstgab = aucstg
     C                   eval      cudbab = aucudb
     C                   eval      ccliab = auccli
     C                   eval      nordab = aunord
     C                   eval      nrigab = aunrig
     C                   eval      srigab = ausrig
     C                   eval      noprab = e6nopr
     C                   eval      roprab = e6ropr
     C                   eval      srprab = e6srpr
     C                   eval      ccpaab = $ccpa
     C                   eval      stapab = e6stat
     C                   eval      msgeab = 'SNS8808'
     C                   exsr      Errore
     C                   iter
     C                   endif
     *
     * Verifico magazzino che deve appartenere all'UDB locale!
     C     e6masm        chain     magan02k
     C                   if        %found
     C                             and a003bz <> ##cudb
     C                   eval      $errore = 'E'
     C                   clear                   zbferr
     C                   eval      uborab = S1ubor
     C                   eval      tdorab = S1tdor
     C                   eval      pdorab = S1pdor
     C                   eval      ndorab = S1ndor
     C                   eval      ubptab = S1ubpt
     C                   eval      protab = S1prot
     C                   eval      nrorab = S1nror
     C                   eval      cstgab = aucstg
     C                   eval      cudbab = aucudb
     C                   eval      ccliab = auccli
     C                   eval      nordab = aunord
     C                   eval      nrigab = aunrig
     C                   eval      srigab = ausrig
     C                   eval      noprab = e6nopr
     C                   eval      roprab = e6ropr
     C                   eval      srprab = e6srpr
     C                   eval      ccpaab = $ccpa
     C                   eval      magsab = e6masm
     C                   eval      msgeab = 'SNS3486'
     C                   exsr      Errore
     C                   iter
     C                   endif
     *
     * Se tutto va bene Aggiorno quantit{ di ORPRA
     C                   eval      e6qlan = aucqta
     C                   eval      e6q01  = aua01
     C                   eval      e6q02  = aua02
     C                   eval      e6q03  = aua03
     C                   eval      e6q04  = aua04
     C                   eval      e6q05  = aua05
     C                   eval      e6q06  = aua06
     C                   eval      e6q07  = aua07
     C                   eval      e6q08  = aua08
     C                   eval      e6q09  = aua09
     C                   eval      e6q10  = aua10
     C                   eval      e6q11  = aua11
     C                   eval      e6q12  = aua12
     C                   eval      e6q13  = aua13
     C                   eval      e6q14  = aua14
     C                   eval      e6q15  = aua15
     C                   eval      e6q16  = aua16
     *
     * Aggiorno ORPRA con status SD: NO BLK
     C                   if        e6cblk = *blanks
     C                   if        e6qlan <> *zeros
     C                   eval      e6stat = 'SD'
     C                   else
     C                   eval      e6stat = 'AN'
     C                   endif
     C                   endif
     *
     * Aggiorno ORPRA con status: BLK
     C                   if        e6cblk <> *blanks
     C                   Clear                   zbf042
     C                   Eval      capibu = s1toqn
     C                   Exsr      SAL_ORPRA
     C                   select
     C                   when      e6qlan = 0
     C                   eval      e6stat = 'AN'
     C                   when      FSALBU = 'Y'
     C                   eval      e6stat = 'SD'
     C                   endSL
     C                   endif
     *
     C     e6stat        chain     evean01k
     C                   eval      e6stan = cgstan
 "   *
     C                   exsr      cabfic
     C                   update    ORPRA01
 "   *
     C                   enddo
 "   *
BET  c                   EndSr
BET  ************************************************
 "   *    SAL_ORPRA - Chiamata al pGM controllo ORTRI
 "   ************************************************
 "   C     SAL_ORPRA     BegSr
 "   *
     C                   Eval      cstgbu = e6cstg
     C                   Eval      cudbbu = e6cudb
     C                   Eval      cclibu = e6ccli
     C                   Eval      nordbu = e6nord
     C                   Eval      nrigbu = e6nrig
     C                   Eval      srigbu = e6srig
     C                   Eval      noprbu = e6nopr
     C                   Eval      roprbu = e6ropr
     C                   Eval      srprbu = e6srpr
     C                   Eval      ccppbu = e6ccpp
     C                   Eval      kpjbu  = zbf042
     C                   Call      'BF042'
     C                   Parm                    KPJBA
     C                   Eval      zbf042 = kpjbu
 "   *
BET  c                   EndSr
BET  ************************************************
 "   *    chi_cl : Chiusura conto lavoro
 "   ************************************************
 "   C     chi_cl        BegSr
 "   *
     c                   if        $cudf=*blank and $cfor=*blank
     c                   eval      $cudf=e6cudb
     c                   eval      $cfor=e6cfor
     c                   endif
     c
     C     kortri        SETGT     ortri07k
     C     kortri        READPE    ortri07k
 "   *
 "   *-- Per non trovato restituisco un errore
     C                   if        %eof(ortri07k)
 "   C                   eval      $errore = 'E'
     C                   clear                   zbferr
     C                   eval      uborab = S1ubor
     C                   eval      tdorab = S1tdor
     C                   eval      pdorab = S1pdor
     C                   eval      ndorab = S1ndor
     C                   eval      ubptab = S1ubpt
     C                   eval      protab = S1prot
     C                   eval      nrorab = S1nror
     C                   eval      msgeab = 'SNS8809'
     C                   exsr      Errore
     C                   eval      S1flan = 'E'
     C                   leavesr
     C                   endif
      *
     C                   if        $tilav<>sdclav or
     C                             sdcfbp<>$ucfbp
 "   C                   eval      $errore = 'E'
     C                   clear                   zbferr
     C                   eval      uborab = S1ubor
     C                   eval      tdorab = S1tdor
     C                   eval      pdorab = S1pdor
     C                   eval      ndorab = S1ndor
     C                   eval      ubptab = S1ubpt
     C                   eval      protab = S1prot
     C                   eval      nrorab = S1nror
     C                   eval      msgeab = 'SNS8669'
     C                   exsr      Errore
     C                   eval      S1flan = 'E'
     C                   leavesr
     C                   endif
      *
 "   *
 "   *--Se il documento } gi{ stato ricevuto, BLOCCO!!!
     C                   eval      ihndoc = s1ndoc
     C     KORTRD        CHAIN     ORTRD06K
     C                   if        %found(ortrd06k)
 "   C                   eval      $errore = 'E'
     C                   clear                   zbferr
     C                   eval      uborab = S1ubor
     C                   eval      tdorab = S1tdor
     C                   eval      pdorab = S1pdor
     C                   eval      ndorab = S1ndor
     C                   eval      ubptab = S1ubpt
     C                   eval      protab = S1prot
     C                   eval      nrorab = S1nror
     C                   eval      msgeab = 'SNS8748'
     C                   exsr      Errore
     C                   eval      S1flan = 'E'
     C                   leavesr
     C                   endif
     *
 "   *--Chiamo il modulo chiusura C/L
     C                   Exsr      MODCL
     *
     C                   if        critai = 'E1'
 "   C                   eval      $errore = 'E'
     C                   clear                   zbferr
     C                   eval      uborab = S1ubor
     C                   eval      tdorab = S1tdor
     C                   eval      pdorab = S1pdor
     C                   eval      ndorab = S1ndor
     C                   eval      ubptab = S1ubpt
     C                   eval      protab = S1prot
     C                   eval      nrorab = S1nror
     C                   eval      msgeab = msgeai
     C                   exsr      Errore
     C                   eval      S1flan = 'E'
     C                   leavesr
     C                   endif
 "   *
BET  C                   EndSr
BET  ************************************************
 "   *    MODCL  : Chiamata al modulo di C/L
 "   ************************************************
 "   C     MODCL         BegSr
 "   *
     C                   clear                   zcl016
     C                   eval      cudbai = aucudb
     C                   eval      ccliai = auccli
     C                   eval      cstgai = aucstg
     C                   eval      nordai = aunord
     C                   eval      nrigai = aunrig
     C                   eval      srigai = ausrig
     C                   eval      noprai = aunord
     C                   eval      roprai = aunrig
     C                   eval      srprai = ausrig
     C                   eval      ccppai = e6ccpa
     C                   eval      damoai = $$GGMMAAAA
     C                   eval      dnsnai = S1ndoc
     C                   eval      dbolai = S1data
     C                   If        Aucblk = '    '
     C                   eval      udmdai =##cudb
     C                   eval      magdai ='PREMAG'
     C                   EndIf
     C                   eval      comiai = 'N'
     C                   eval      naveai = 'Y'
BET  *
BET  *--Utilizzo la schiera trascodificata CSIS
     C                   eval      qtotai = s1toqn
     C                   eval      qtr01  = rlns(1)
     C                   eval      qtr02  = rlns(2)
     C                   eval      qtr03  = rlns(3)
     C                   eval      qtr04  = rlns(4)
     C                   eval      qtr05  = rlns(5)
     C                   eval      qtr06  = rlns(6)
     C                   eval      qtr07  = rlns(7)
     C                   eval      qtr08  = rlns(8)
     C                   eval      qtr09  = rlns(9)
     C                   eval      qtr10 =  rlns(10)
     C                   eval      qtr11 =  rlns(11)
     C                   eval      qtr12 =  rlns(12)
     C                   eval      qtr13 =  rlns(13)
     C                   eval      qtr14 =  rlns(14)
     C                   eval      qtr15 =  rlns(15)
     C                   eval      qtr16 =  rlns(16)
     *
     *--imposto il flag di chiusra C/L solo se non } bulk, oppure se bulk
     *--solo se vado a saldare.
     C                   eval      fchiai = 'N'
BET  C                   If        (aucblk <> '   ' and aucqtm >= e6qlan)
BET  C                             or aucblk = *blanks
     C                   eval      fchiai = 'Y'
BET  C                   EndIf
     *
BET  C                   Eval      tlavai = sdclav
     C                   EVAL      udbtai = sdtudb
     C                   EVAL      cforai = sdcfor
     *
     C                   eval      kpjbu =zcl016
     C                   call      'CL016A'
     C                   parm                    kpjba
     *
     C                   eval      zcl016 = kpjbu
     *
BET  c                   EndSr
BET  ************************************************
 "   *    ava_pro: Avanzamenti produttivi
 "   ************************************************
 "   C     ava_pro       BegSr
     *
 "   *--Leggo tutto ORPRA
     C     KorpraSNS     setll     orpra01k
     C                   do        *hival
     C     KorpraSNS     reade(N)  orpra01k
 "   *
     C                   if        %EOF
     C                   leave
     C                   endif
 "   *
 "   C                   Clear                   zsn680
 "   C                   Eval      $_pgmci2 = 'BF003L'
 "   C                   Eval      $_FICOI2 = 'Y'
 "   C                   Eval      $_GECOI2 = 'N'
 "   C                   Eval      $_CUDBI2 = AUcudb
 "   C                   Eval      $_CCLII2 = AUccli
 "   C                   Eval      $_CSTGI2 = AUcstg
 "   C                   Eval      $_NORDI2 = AUnord
 "   C                   Eval      $_NRIGI2 = AUnrig
 "   C                   Eval      $_SRIGI2 = AUsrig
 "   C                   Eval      $_NOPRI2 = AUnord
 "   C                   Eval      $_ROPRI2 = AUnrig
 "   C                   Eval      $_SRPRI2 = AUsrig
BET  *
 "   C                   Eval      $_QTATI2 = s1toqn
 "   C                   Eval      $_QTA01  = rlns(1)
 "   C                   Eval      $_QTA02  = rlns(2)
 "   C                   Eval      $_QTA03  = rlns(3)
 "   C                   Eval      $_QTA04  = rlns(4)
 "   C                   Eval      $_QTA05  = rlns(5)
 "   C                   Eval      $_QTA06  = rlns(6)
 "   C                   Eval      $_QTA07  = rlns(7)
 "   C                   Eval      $_QTA08  = rlns(8)
 "   C                   Eval      $_QTA09  = rlns(9)
 "   C                   Eval      $_QTA10  = rlns(10)
 "   C                   Eval      $_QTA11  = rlns(11)
 "   C                   Eval      $_QTA12  = rlns(12)
 "   C                   Eval      $_QTA13  = rlns(13)
 "   C                   Eval      $_QTA14  = rlns(14)
 "   C                   Eval      $_QTA15  = rlns(15)
 "   C                   Eval      $_QTA16  = rlns(16)
 "   *
 "   C                   Eval      $_CCPPI2 = e6ccpp
 "   C                   Eval      $_CCP1I2 = AUCCPM
 "   C**$                Eval      $_CFB1I2 = sdcfbp
 "   C                   Eval      $_TOP1I2 = 'SCA'
 "   *
 "   *  Carico la fase riprendendola dal tipo lavorazione
     C*                  exsr      fas_pro
     C                   exsr      cal_fase
 "   C                   Eval      $_CFP1I2 = a2cele
 "   C                   Eval      $_CFB1I2 = sdcfbp
 "   C                   If        a2cele = *blanks
 "   C**                 eval      $errore = 'E'
 "   C                   clear                   zbferr
 "   C                   eval      cstgab = aucstg
 "   C                   eval      cudbab = aucudb
 "   C                   eval      ccliab = auccli
 "   C                   eval      nordab = aunord
 "   C                   eval      nrigab = aunrig
 "   C                   eval      srigab = ausrig
 "   C                   eval      ccpaab = auccpm
 "   C                   eval      msgeab = 'SNS8604'
 "   C**                 exsr      Errore
 "   C                   leavesr
 "   C                   EndIf
 "   *
 "   *--Se porto la merce a magazzino scarico le q.ta ricevute
 "   C                   Eval      $_CCP2I2 = AUCCPM
 "   C                   Eval      $_CFB2I2 = sdcfbp
 "   C                   Eval      $_CFP2I2 = a2cele
 "   C                   Eval      $_TOP2I2 = 'SCR'
 "   *
 "   C                   Eval      $kpjbu   = kpjbu
 "   C                   Eval      kpjbu    = zsn680
 "   *
 "   C                   Call      'SN680'
 "   C                   Parm                    kpjba
 "   *
 "   C                   Eval      zsn680   = kpjbu
 "   C                   Eval      kpjbu    = $kpjbu
 "   *
 "   C                   If        $_criti2 = 'ER'
 "   C**                 eval      $errore = 'E'
 "   C                   clear                   zbferr
 "   C                   eval      cstgab = aucstg
 "   C                   eval      cudbab = aucudb
 "   C                   eval      ccliab = auccli
 "   C                   eval      nordab = aunord
 "   C                   eval      nrigab = aunrig
 "   C                   eval      srigab = ausrig
 "   C                   eval      ccpaab = auccpm
 "   C                   eval      msgeab = $_cmsgi2
 "   C**                 exsr      Errore
 "   C                   leavesr
 "   C                   EndIf
 "   *
 "   *--Se porto la merce a magazzino carico le q.ta ricevute
 "   C                   Eval      $_CCP1I2 = auccpm
 "   C                   Eval      $_CPA1I2 = E6CCPA                            **LEL**
 "   C                   Eval      $_UDB1I2 = ##cudb
 "   C                   Eval      $_CFP1I2 = h5fmag
 "   C                   Eval      $_TOP1I2 = 'CAR'
 "   C                   Eval      $_CCP2I2 = *Blanks
 "   C                   Eval      $_UDB2I2 = *Blanks
 "   C                   Eval      $_CFP2I2 = *Blanks
 "   C                   Eval      $_TOP2I2 = *Blanks
 "   *
 "   C                   Eval      $kpjbu   = kpjbu
 "   C                   Eval      kpjbu    = zsn680
 "   *
 "   C**                 Call      'SN680'
 "   C**                 Parm                    kpjba
 "   *
 "   C                   Eval      zsn680   = kpjbu
 "   C                   Eval      kpjbu    = $kpjbu
 "   *
 "   C                   If        $_criti2 = 'ER'
 "   C**                 eval      $errore = 'E'
 "   C                   clear                   zbferr
 "   C                   eval      cstgab = aucstg
 "   C                   eval      cudbab = aucudb
 "   C                   eval      ccliab = auccli
 "   C                   eval      nordab = aunord
 "   C                   eval      nrigab = aunrig
 "   C                   eval      srigab = ausrig
 "   C                   eval      ccpaab = auccpm
 "   C                   eval      msgeab = $_cmsgi2
 "   C**                 exsr      Errore
 "   C                   leavesr
 "   C                   EndIf
 "   *
 "   C                   EndDO
 "   *
BET  c                   EndSr
BET  ************************************************
 "   *    fas_pro: CARICO FASE PRODUTTIVA
 "   ************************************************
 "   C     fas_pro       BegSr
     *
 "   C                   clear                   z@cfpd
     C                   clear                   a2cele
     *
 "   C     'FPD'         Setll     tsian99k
 "   C                   Do        *Hival
 "   C     'FPD'         ReadE     tsian99k
 "   *
 "   C                   If        %EOF(tsian99k)
 "   C                   clear                   z@cfpd
     C                   clear                   a2cele
 "   C                   Leave
 "   C                   EndIf
 "   *
 "   C                   Eval      z@cfpd = a2xtab
 "   *
 "   C                   If        ctilib <> sdclav
 "   C                   Iter
 "   C                   EndIf
 "   *
     C                   leave
 "   *
     C                   enddo
 "   *
BET  c                   EndSr


BET  ************************************************
 "   *    Trovo fase ultima
 "   ************************************************
 "   C     cal_fase      BegSr
     *
 "   C                   clear                   z@cfpd
     C                   clear                   a2cele
     *
dla  c                   if        sdclav=*blanks  and
     C                             ifnewa<> 'Y'
     c                   exsr      sFasUltAVPRD
     c                   LeaveSr
     c                   endif
c    *
 "   C     'FPD'         Setll     tsian99k
 "   C                   Do        *Hival
 "   C     'FPD'         ReadE     tsian99k
 "   *
 "   C                   If        %EOF(tsian99k)
 "   C                   clear                   z@cfpd
     C                   clear                   a2cele
 "   C                   Leave
 "   C                   EndIf
 "   *
 "   C                   Eval      z@cfpd = a2xtab
 "   *
 "   C                   If        ctilib <> sdclav
 "   C                   Iter
 "   C                   EndIf
 "   *
     C                   leave
 "   *
     C                   enddo
 "   *
BET  c                   EndSr


dla  ************************************************
 "   * sFasUltAVPRD - rtv fase da ultimo avprd
 "   ************************************************
     c     sFasUltAVPRD  Begsr
 "   *
      *- PRENDE FASE /FABBRICA DA ULT. fase :
 "   C                   Eval      ICPROG = *HIVAL
     C     kavprd03      SETLL     AVPRD03K
     C     kavprd03x     READPE    AVPRD03K
     c                   if        not %eof(AVPRD03K)
 "   C                   Eval      a2cele = $iccfpd
 "   C                   Eval      sdcfbp = $iccfbp
     c                   endif
 "   *
     c                   EndSr

BET9 ************************************************
 "   * Maglieria - Controllo se chiudere ultima fase
 "   *             o collaudo
 "   ************************************************
 "   C     Maglieria     BegSr
 "   *
     C                   IF        aucblk = *blanks and AUCQTA > S1TOQN
BET9 C                             and colvir<>'Y'
     *--Aggiornamento per ORCRI NO BULK: preannullamenti, quantit{, status, e date
 "   C     Kriga         Chain     orcri28k
 "   C                   If        not %found
 "   C     Krigap        Chain     orcri28k
 "   C                   EndIf
     C                   exsr      preann_lel
     c                   Endif
 "   *
 "   *
     C     S1ubus        chain     udban03k

 "   * Leggo le righe di AVPRD da chiudere fino a soddisfare
 "   * le quantita ricevute da CSIS
 "   C                   EVAL      RLNSX= RLNS
     c                   if        $agnon<>'N'
     C                   EXSR      AVANZAM
     C                   IF        $errore <>  *blanks
 "   C                   leavesr
     C                   EndIf
     C                   EndIf
 "
 "   * Se non sono riuscito a trovare corrispondenza tra fase e AVPRD, allora seno arrivati
 "   * i capi ma non } stato eseguito correttamente l'avanzamento produttivo.
     C                   IF        $CTL_AVPR = *blanks  and $agnon<>'N'
 "   C                   eval      $errore = 'E'
 "   C                   clear                   zbferr
 "   C                   eval      uborab = S1ubor
 "   C                   eval      tdorab = S1tdor
 "   C                   eval      pdorab = S1pdor
 "   C                   eval      ndorab = S1ndor
 "   C                   eval      ubptab = S1ubpt
 "   C                   eval      protab = S1prot
 "   C                   eval      nrorab = S1nror
 "
 "   C                   eval      CSTGAB = e6CSTG
 "   C                   eval      CUDBAB = e6CUDB
 "   C                   eval      CCLIAB = e6CCLI
 "   C                   eval      NORDAB = e6NORD
 "   C                   eval      NRIGAB = e6NRIG
 "   C                   eval      SRIGAB = e6SRIG
 "   C                   eval      NOPRAB = e6NOPR
 "   C                   eval      ROPRAB = e6ROPR
 "   C                   eval      SRPRAB = e6SRPR
 "   C                   eval      CCPPAB = e6ccpa
 "   C                   eval      CFAMAB = e6cfam
 "
 "   C                   eval      msgeab = 'SNS8979'
 "   C                   eval      ccpaab = e6ccpa
 "   C                   exsr      Errore
     C                   leavesr
     C                   endif
     C                   if        S1stnw='CLD'
     C                   exsr      CL_ctlform
     C                   if        $errore <> *blanks
     C                   leavesr
     C                   endif
     C                   endif

 "   * Controllo di aver chiuso fasi sufficienti a soddisfare TUTTE
 "   * le quantita ricevute da CSIS
 "   ****************  NON PIU ESEGUITA - Ora gestisce archivio differenze MGDIF *********
     C                   IF        1=2
 "   C                   If        $QTATOTALE > *Zeros AND
 "   C                             $Errore <> 'E' and S1stnw <> '*L*'
 "   C                   eval      $errore = 'E'
 "   C                   clear                   zbferr
 "   C                   eval      uborab = S1ubor
 "   C                   eval      tdorab = S1tdor
 "   C                   eval      pdorab = S1pdor
 "   C                   eval      ndorab = S1ndor
 "   C                   eval      ubptab = S1ubpt
 "   C                   eval      protab = S1prot
 "   C                   eval      nrorab = S1nror
 "
 "   C                   eval      CSTGAB = e6CSTG
 "   C                   eval      CUDBAB = e6CUDB
 "   C                   eval      CCLIAB = e6CCLI
 "   C                   eval      NORDAB = e6NORD
 "   C                   eval      NRIGAB = e6NRIG
 "   C                   eval      SRIGAB = e6SRIG
 "   C                   eval      NOPRAB = e6NOPR
 "   C                   eval      ROPRAB = e6ROPR
 "   C                   eval      SRPRAB = e6SRPR
 "   C                   eval      CCPPAB = e6ccpa
 "   C                   eval      CFAMAB = e6cfam
 "
 "   C                   eval      msgeab = 'SNS8903'
 "   C                   eval      ccpaab = e6ccpa
 "   C                   exsr      Errore
     C                   leavesr
 "   C                   EndIf
 "   C                   ENDIF
 "
 "   * verifica se ci sono qta residue da aggiornare su file MGDIF (caso collaudo)
 "   *          o da generare la sovraproduzione (caso ultima faser reale senza collaudo)
     C                   XFOOT     LTQ$          $TOTLTQ
     C                   If        $TOTLTQ>0
 "   C                   if        $FCOL =  'Y'                                 Fase Collaudo
 "   C                   exsr      Qtadiffe
     C                   else
     C                   exsr      cabfic
     c                   update    rorcri
 "   C                   exsr      Sovraprod
     C                   EXSR      AVANZAM
 "   C                   EVAL      RLNS= RLNSX
     C                   IF        $errore <>  *blanks
 "   C                   leavesr
     C                   EndIf
     C                   endif
 "   C                   if        $errore = 'E'
     C                   leavesr
     C                   endif
 "   C                   EndIf
PG1   *-------------------------------------------
PG1  C                   if        $assegna<>'1'
     *--Aggiornamento ORPRA
     C                   exsr      Agg_orpra
PG1  C                   Endif
PG1   *-------------------------------------------
 "   *
 "   *--Aggiornamento ORCRI
 "   C     Kriga         Chain     orcri28k
 "   C                   If        not %found
 "   C     Krigap        Chain     orcri28k
 "   C                   EndIf
PG1   * --------------------------------------------
PG1   * Se cravatta aggiorno legami fissi
PG1  c                   if        $assegna='1'
PG1  c                   exsr      mag_leg
PG1  c                   EndIf
PG1   * --------------------------------------------
      *
 "   C                   if        s1stol='VEN'  and s1stnw='VAS'
     C                   exsr      agg_qtarv
     C                   exsr      agg_qmagv
     C                   Endif
 "   *
 "   *--Aggiornamento Orcri status
     C                   if        aucblk = *blanks or $assegna='1'
 "   C                   EVAL      RLNS= RLNSX
     C                   exsr      agg_qmag
PG1   * ----------------------------------------------------------
 "    * Solo se qt.a versate uguale a qta.in essere allora imposto
 "    * stato versato
 "   c                   if        $assegna='1'
 "   * ------------------------------------------------
 "   * solo se versato uguale a essere (totale versato)
 "   c                   if        AUCQTM = AUCQTA
 "   c                   eval      audtma = $$dat8
 "   c                   eval      audtst = $$dat8
 "   c                   if        austan<='200'
 "   c                   eval      austat = 'RM'
 "   c                   endif
 "   *
 "   c                   endif
 "   *
 "   c                   else
 "   *
 "   c                   eval      audtma = $$dat8
 "   c                   eval      audtst = $$dat8
 "   c                   if        austan<='200'
 "   c                   eval      austat = 'RM'
 "   c                   endif
 "   *
 "   c                   endif
PG1  *
     C                   Else
 "   *
     C                   Clear                   zmg039
     C                   Eval      cudba4 = aucudb
     C                   Eval      cstga4 = aucstg
     C                   Eval      cclia4 = auccli
     C                   Eval      norda4 = aunord
     C                   Eval      nriga4 = aunrig
     C                   Eval      sriga4 = ausrig
     C                   Eval      kpjbu = Zmg039
     C                   Call      'MG039'
     C                   Parm                    KPJBA
     C                   Eval      zmg039 = kpjbu
 "   *
     C                   if        crita4 = *blanks
 "   C                   eval      audtma = $$dat8
 "   C                   eval      audtst = $$dat8
     c                   if        austan<='200'
     c                   eval      austat = 'RM'
     C                   endif
     C                   endif
     *
     C                   endif
     *
     C     austat        chain     evean01k
     C                   eval      austan = cgstan
     *
     C                   exsr      cabfic
     C                   update    rorcri
     c
PG1  * ----------------------------------------------
 "   * Aggirono dopo orcri cosi ho qta. a magazzino
 "   * aggirnate
 "   *--Aggiornamento ORPRA
 "    *-------------------------------------------
 "   c                   if        $assegna='1'
 "   c                   exsr      Agg_orpra
 "   c                   Endif
PG1  * ----------------------------------------------
     *
BET9 c                   EndSr

PG1  ************************************************
 "   * Aggiorno qta di legate di magazzino su legami fissi
 "   ************************************************
 "   c     Mag_leg       Begsr
 "   c
 "   c     klegor        setll     legor01k
 "   c                   do        *hival
 "   c     klegor        reade     legor01k
 "   c
 "   c                   if        %eof(legor01k)
 "   c                   leave
 "   c                   endif
 "    *
 "   c                   if        hmlege=*blank
 "   c                   iter
 "   c                   endif
 "    *
 "   c                   eval      HMQMAG=HMQMAG+s1toqn
 "   c                   eval      HMQMAV=HMQMAV+s1toqn
 "   c                   eval      hmm01  = RLNX(1) + hmm01
 "   c                   eval      hmm02  = RLNX(2) + hmm02
 "   c                   eval      hmm03  = RLNX(3) + hmm03
 "   c                   eval      hmm04  = RLNX(4) + hmm04
 "   c                   eval      hmm05  = RLNX(5) + hmm05
 "   c                   eval      hmm06  = RLNX(6) + hmm06
 "   c                   eval      hmm07  = RLNX(7) + hmm07
 "   c                   eval      hmm08  = RLNX(8) + hmm08
 "   c                   eval      hmm09  = RLNX(9) + hmm09
 "   c                   eval      hmm10  = RLNX(10)+ hmm10
 "   c                   eval      hmm11  = RLNX(11)+ hmm11
 "   c                   eval      hmm12  = RLNX(12)+ hmm12
 "   c                   eval      hmm13  = RLNX(13)+ hmm13
 "   c                   eval      hmm14  = RLNX(14)+ hmm14
 "   c                   eval      hmm15  = RLNX(15)+ hmm15
 "   c                   eval      hmm16  = RLNX(16)+ hmm16
 "    *
 "   c                   eval      hmv01  = RLNX(1) + hmv01
 "   c                   eval      hmv02  = RLNX(2) + hmv02
 "   c                   eval      hmv03  = RLNX(3) + hmv03
 "   c                   eval      hmv04  = RLNX(4) + hmv04
 "   c                   eval      hmv05  = RLNX(5) + hmv05
 "   c                   eval      hmv06  = RLNX(6) + hmv06
 "   c                   eval      hmv07  = RLNX(7) + hmv07
 "   c                   eval      hmv08  = RLNX(8) + hmv08
 "   c                   eval      hmv09  = RLNX(9) + hmv09
 "   c                   eval      hmv10  = RLNX(10)+ hmv10
 "   c                   eval      hmv11  = RLNX(11)+ hmv11
 "   c                   eval      hmv12  = RLNX(12)+ hmv12
 "   c                   eval      hmv13  = RLNX(13)+ hmv13
 "   c                   eval      hmv14  = RLNX(14)+ hmv14
 "   c                   eval      hmv15  = RLNX(15)+ hmv15
 "   c                   eval      hmv16  = RLNX(16)+ hmv16
 "   c                   exsr      cabfic
 "   c                   update    rlegor
 "    *
 "   c                   leave
 "    *
 "   c                   Enddo
 "    *
PG1  c                   Endsr
BET9 ************************************************
 "   * AVANZAM
 "   ************************************************
 "   C     AVANZAM       BegSr
 "   *
 "   C                   Eval      LTQ$     = RLNS
 "   C                   Eval      RLNS$ = RLNS
 "   C                   Eval      $QTATOTALE = S1TOQN
 "   C                   Clear                   z@cawb
 "   C                   Movel(P)  'AWB'         $ctab
 "   C                   Movel(P)  APCUDB        $cele
 "   C                   Exsr      Ctl_decotab
 "   C                   If        @DLI3 <> '1'
 "   C                   Movel(P)  @dli2         z@cawb
 "   C                   EndIf
 "
 "   C                   movel     *blanks       $CTL_AVPR         1

 "   * Leggo le righe di AVPRD da chiudere fino a soddisfare
 "   * le quantita ricevute da CSIS
PG1  * ------------------------------------------------------
PG1  * Sostituita lf AVPRD18K on AVPRD11K  leggendo dal piu alto
PG1  * al piu basso cosi da prendere le ultime fasi
PG1  * ------------------------------------------------------
PG1  C**   Kavprd18k     Setll     avprd18k
PG1  C     Kavprd18k     Setgt     avprd11k
 "   C                   Do        *hival
 "
PG1  C**   Kavprd18k     ReadE     avprd18k
PG1  C**                 If        %EOF(avprd18k)
PG1  C     Kavprd18k     Readpe    avprd11k
PG1  C                   If        %EOF(avprd11k)
 "   C                   Leave
 "   C                   EndIf
 "
 "   * Decodifico fase
 "   C                   Clear                   z@cfpd
 "   C                   Movel(P)  'FPD'         $ctab
 "   C                   Movel(P)  ICCFPD        $cele
 "   C                   Exsr      Ctl_decotab
 "   C                   If        @DLI3 <> '1'
 "   C                   Movel(P)  @dli2         z@cfpd
 "   C                   EndIf
 "
 "   * Verifico se devo chiudere ultima fase reale o fase virtuale di collaudo
 "   C                   Select
 "
 "   C                   When      cfpdlk <> *Blanks AND
 "   C                             iccfpd =  cfpdlk  AND
 "   C                             fcolib =  'Y'     AND                        Fase Collaudo
 "   C                             fvirib =  'Y'                                Fase Virtuale
 "   C                   eval      $CTL_AVPR = '*'
 "   C                   ExSr      Chiudi_fase
     C                   IF        $errore <>  *blanks
 "   C                   leavesr
     C                   EndIf
 "
 "   C                   When      cfpdlk =  *Blanks AND
 "   C                             fvirib <> 'Y'     AND                        Fase Reale
 "   C                             iccfps =  *Blanks                            Fase Successiva
 "   C                   eval      $CTL_AVPR = '*'
 "   C                   ExSr      Chiudi_fase
     C                   IF        $errore <>  *blanks
 "   C                   leavesr
     C                   EndIf
 "
 "   C                   EndSl
 "
 "   C                   If        $QTATOTALE = *Zeros
 "   C                   Leave
 "   C                   EndIf
 "
 "   C                   EndDo
 "
BET9 c                   EndSr
 "   ************************************************
 "   * Chiudi_fase - Richiamo modulo chiusura fase per
 "   *               maglieria
 "   ************************************************
 "   C     Chiudi_fase   BegSr
 "   *
 "   C                   Eval      $Trovato = *OFF
BET5 C                   Eval      $PrimaCPP= *ON
     c     Kfapes        chain     fapes01k
 "   *
     c                   if        fcolib =  'Y'  and fvirib<>'Y'
      * se fase collaudo reale interno va sul rientrato se no sullo spedito
     c                   if        utfbin='Y'
 "   C                   Eval      ICR$     = *Zeros
     c                   else
 "   C                   Eval      ICZ$     = *Zeros
     c                   endif
      *
     c                   else
 "   C                   Eval      ICE$     = *Zeros
     C                   ENDIF
 "   *
     C                   EVAL      $CFPD=*BLANKS
     C                   EVAL      $CFBP=*BLANKS
     C                   EVAL      $GRPF=*ZEROS
     C                   EVAL      $FCOL=*BLANKS
 "
     * Verifica se devo eseguire la subroutine (x casi pdc multiple):
     C                   If        icccpa <> *blanks                            *LEL*
     c                   Eval      $$ccpa = icccpa                              *LEL*
     C                   Else                                                   *LEL*
     c                   Eval      $$ccpa = icccpp                              *LEL*
     C                   EndIf                                                  *LEL*
     * Escludo dal giro eventuali avprd di componenti (sono gia saldati):
     C     $$ccpa        chain     cpman01k
     c                   IF        %found(cpman01k)
     c                             and acfgcm = 'Y'
 "   C                   LeaveSr
     c                   endif
     *
     * eseguo solo x la prima cpp in caso pdc multiple , poi bisognera risolvere come fare a
     *   chiudere gli avprd delle altre cpp !!!!!!!!!!!!!!!!!!!!!!! **las**
     C                   exsr      sTestEsegui                                  *las*
     c                   if        not $esegui                                  *las*
 "   C                   LeaveSr
     C                   endif                                                  *las*
     *
     c                   exsr      sCalcQtaRes                                  *las*
 "   C                   If        NOT $trovato
 "   C                   LeaveSr
 "   C                   EndIf
 "
     *
     c                   if        $$ccpa=ICccpp
     c                   eval      $$ccpp=icccpp
     c                   exsr      sCallMG400
     C                   LeaveSr
     c                   endif
     *
     * x eseguire le altre pdc appartenenti alla CPA:
     *
     C     $$ccpa        SETLL     cpmad01k
     c                   Do        *hival
     C     $$ccpa        READE     cpmad01k
     c                   if        %eof
     C                   LeaveSr
     c                   endif
     * chiamo MG400 x tutte le parti di capo comprese nella cpa
     c                   eval      $$ccpp=adccpp
     C     kavprd03cp    setll     avprd03k
     c                   do        *hival
     C     kavprd03cp    reade     avprd03k
     c                   if        %eof
     C                   LeaveSr
     c                   endif
     c                   if        $icstat='SD'
     c                   iter
     c                   endif
     c                   eval      avprd00f=AVPRD03ds
     c                   exsr      sCallMG400
     c                   leave
     c                   enddo
BET5 C                   Eval      $PrimaCPP= *OFF
     c                   Enddo
 "   *
BET9 c                   EndSr
BET9 ************************************************
 "   * Mod_ann_blk - Richiamo modulo aggiornamento
 "   *               annullamento per ordini bulk
 "   *               richiesta CER del 29.01.2009
 "   ************************************************
 "   C     Mod_ann_blk   BegSr
 "   *
     C                   Movel(P)  'TPO'         $ctab
     C                   Movel(P)  autord        $cele
     C                   Exsr      Ctl_decotab
 "   C                   If        @DLI3 <> '1'
 "   C                   Movel(P)  @dli2         z@tord
 "   C                   EndIf
 "   *
     c                   IF        FSBL3A = 'Y'
     c                   clear                   zor318
     c                   eval      $ORUDBSBC = aucudb
     c                   eval      $ORCLISBC = auccli
     c                   eval      $ORSTGSBC = aucstg
     c                   z-add     aunord        $ORnordbc
     c                   z-add     aunrig        $ornrigbc
     c                   z-add     aua01         $orQT01
     c                   z-add     aua02         $orQT02
     c                   z-add     aua03         $orQT03
     c                   z-add     aua04         $orQT04
     c                   z-add     aua05         $orQT05
     c                   z-add     aua06         $orQT06
     c                   z-add     aua07         $orQT07
     c                   z-add     aua08         $orQT08
     c                   z-add     aua09         $orQT09
     c                   z-add     aua10         $orQT10
     c                   z-add     aua11         $orQT11
     c                   z-add     aua12         $orQT12
     c                   z-add     aua13         $orQT13
     c                   z-add     aua14         $orQT14
     c                   z-add     aua15         $orQT15
     c                   z-add     aua16         $orQT16
     c                   eval      $ORCOMMBC = 'N'
     c                   eval      Kpjbu = ZOR318
     c                   call      'OR318'
     c                   parm                    Kpjba
     c                   Endif
BET9 c                   EndSr
BET9 ************************************************
 "   * Qtadiffe    - aggiorna MGDIF x differenze riscontrate
 "   ************************************************
 "   C     Qtadiffe      BegSr
 "   *
     c     kmgdif        chain     mgdif01k
     c                   if        %found
     C                   eval      $errore = 'E'
     C                   clear                   zbferr
     C                   eval      msgeab = 'SNS9055'
     C                   exsr      Errore
     c                   Leavesr
     c                   endif
 "   *
     C                   clear                   RMGDIF
 "   *
 "   C                   eval      LTCSTG = ICCSTG
 "   C                   eval      LTCUDB = ICCUDB
 "   C                   eval      LTCCLI = ICCCLI
 "   C                   eval      LTNORD = ICNORD
 "   C                   eval      LTNRIG = ICNRIG
 "   C                   eval      LTSRIG = ICSRIG
 "   *
 "   C                   Eval      LTQ01  = LTQ$(01)
 "   C                   Eval      LTQ02  = LTQ$(02)
 "   C                   Eval      LTQ03  = LTQ$(03)
 "   C                   Eval      LTQ04  = LTQ$(04)
 "   C                   Eval      LTQ05  = LTQ$(05)
 "   C                   Eval      LTQ06  = LTQ$(06)
 "   C                   Eval      LTQ07  = LTQ$(07)
 "   C                   Eval      LTQ08  = LTQ$(08)
 "   C                   Eval      LTQ09  = LTQ$(09)
 "   C                   Eval      LTQ10  = LTQ$(10)
 "   C                   Eval      LTQ11  = LTQ$(11)
 "   C                   Eval      LTQ12  = LTQ$(12)
 "   C                   Eval      LTQ13  = LTQ$(13)
 "   C                   Eval      LTQ14  = LTQ$(14)
 "   C                   Eval      LTQ15  = LTQ$(15)
 "   C                   Eval      LTQ16  = LTQ$(16)
 "   *
 "   C                   Eval      LTNBNS = S1NDOC
 "   C                   Eval      LTDBNS = S1DATA
 "
 "   C                   eval      LTCFPD = $CFPD
 "   C                   eval      LTANNO = S1ANNO
 "   C                   eval      LTPRTR = S1PRTR
 "   *
     C                   exsr      cabfic
 "   C                   Z-ADD     PPFECH        LTDAMO
 "   *
     C                   write     RMGDIF
 "   *
BET9 c                   EndSr
BET9 ************************************************
 "   * Sovraprod   - gestione sovraproduzione
 "   ************************************************
 "   C     Sovraprod     BegSr
 "   *
     c                   eval      $sovraprod=*off
 "   *
     c                   clear                   zmg125
     c                   eval      CRITA1='CO'                                  X COMMIT WRKF
     c                   eval      CSTGA1=ICCSTG
     c                   eval      CUDBA1=ICCUDB
     c                   eval      CCLIA1=ICCCLI
     c                   eval      NORDA1=ICNORD
     c                   eval      NRIGA1=ICNRIG
     c                   eval      SRIGA1=ICSRIG
     c                   eval      CCPPA1=ICCCPP
     c                   eval      CFPDA1=$CFPD
     c                   eval      CFBPA1=$CFBP
     c                   eval      GRPFA1=$GRPF
     c                   eval      TISOA1='SP'
      * qta' sovrapprodotte
     c                   eval      QTSPA1=$TOTLTQ
     c                   eval      QSP01=LTQ$(01)
     c                   eval      QSP02=LTQ$(02)
     c                   eval      QSP03=LTQ$(03)
     c                   eval      QSP04=LTQ$(04)
     c                   eval      QSP05=LTQ$(05)
     c                   eval      QSP06=LTQ$(06)
     c                   eval      QSP07=LTQ$(07)
     c                   eval      QSP08=LTQ$(08)
     c                   eval      QSP09=LTQ$(09)
     c                   eval      QSP10=LTQ$(10)
     c                   eval      QSP11=LTQ$(11)
     c                   eval      QSP12=LTQ$(12)
     c                   eval      QSP13=LTQ$(13)
     c                   eval      QSP14=LTQ$(14)
     c                   eval      QSP15=LTQ$(15)
     c                   eval      QSP16=LTQ$(16)
     c
     c                   eval      $kpjbu=kpjbu
     c                   eval      kpjbu=zmg125
     c                   call      'MG125CL'
     c                   parm                    KPJBA
     c
     c                   eval      zmg125=kpjbu
     c                   eval      kpjbu=$kpjbu
     c                   If        crita1  = 'ER'
 "   C                   eval      $errore = 'E'
 "   C                   clear                   zbferr
 "   C                   eval      uborab = S1ubor
 "   C                   eval      tdorab = S1tdor
 "   C                   eval      pdorab = S1pdor
 "   C                   eval      ndorab = S1ndor
 "   C                   eval      ubptab = S1ubpt
 "   C                   eval      protab = S1prot
 "   C                   eval      nrorab = S1nror
 "
 "   C                   eval      CSTGAB = e6CSTG
 "   C                   eval      CUDBAB = e6CUDB
 "   C                   eval      CCLIAB = e6CCLI
 "   C                   eval      NORDAB = e6NORD
 "   C                   eval      NRIGAB = e6NRIG
 "   C                   eval      SRIGAB = e6SRIG
 "   C                   eval      NOPRAB = e6NOPR
 "   C                   eval      ROPRAB = e6ROPR
 "   C                   eval      SRPRAB = e6SRPR
 "   C                   eval      CCPPAB = e6ccpa
 "   C                   eval      CFAMAB = e6cfam
 "
 "   C                   eval      msgeab = cmsga1
 "   C                   eval      ccpaab = e6ccpa
 "   C                   exsr      Errore
     c                   LeaveSr
     c                   EndIf
 "   *
     c                   eval      $sovraprod=*on
 "   *
BET9 c                   EndSr

BETA ************************************************
 "   * CallN782    - Gestione CDPR su matricole PRMAT
 "   ************************************************
 "   C     CallLN782     BegSr
 "
 "   C                   Clear                   zln782
 "   C                   Eval      xPGMCES = 'BF003L'
 "   C                   Eval      xCSTGES = AUCSTG
 "   C                   Eval      xCUDBES = AUCUDB
 "   C                   Eval      xCCLIES = AUCCLI
 "   C                   Eval      xNORDES = AUNORD
 "   C                   Eval      xNRIGES = AUNRIG
 "   C                   Eval      xSRIGES = AUSRIG
 "   * Forza aggiornamento CDPR e EAN code + ORCRI e OVDRG
 "   C                   Eval      xWCDPES = 'Y'
 "   C                   Eval      xFORZES = 'N'
 "   C                   Eval      $kpjbu   = kpjbu
 "   C                   Eval      kpjbu    = zln782
 "
 "   C*                  Call      'LN782CL'
 "   C*                  Parm                    kpjba
 "
 "   C                   Eval      zln782   = kpjbu
 "   C                   Eval      kpjbu    = $kpjbu
 "
BETA c                   EndSr
     *****************************************************************
     * sTestEsegui - test x verifica se deve eseguire la routine (caso CPA<>CPP) x formale
     *****************************************************************
     c     sTestEsegui   begsr
     *
     *  *LAS*     SERVE X QUESTE ROUTINES:
     *  -----------------------------------------    ------------  $$CCPA  -----------------------
     *  Chiamata a :                                 1 cp utilizzata   2 cp utilizzata se mancal la 1
     *  Car Mag fase se no C/L                       ICCCPR            ccppaa (ovvero CPP ricev.)
     *  Sca Mag fase se ultima fase                  ICCCPR            ccppaa (ovvero CPP ricev.)
     *  Sca Mag fabbrica se no C/L                   ICCCPA            ccppaa (ovvero CPP ricev.)
     *  MP810 - fatt.passive                         ICCCPR            ccppaa (ovvero CPP ricev.)
     *  CPA x WRI Ricev.a premag.                    ICCCPA            ICCCPP
     *
     *  deve farlo solo una volta x cpm multiple
     *
     *
     c                   eval      $esegui=*on
     *
     *  se cpa=cpp esegue sempre (caso cp singola):
     c                   if        $$ccpa=ICccpp
     C                   LeaveSr
     c                   endif
     * va sul cpmad x cpa e: - esegue x non trovato (ovvero parte di capo non multipla) oppure
     *                       - esegue solo x la prima parte di capo singola appartenente alla CPA:
     *
     C     $$ccpa        SETLL     cpmad01k
     C     $$ccpa        READE     cpmad01k
     c                   if        %eof
     C                   LeaveSr
     c                   endif
     *
     c                   if        ICccpp=adccpp
     C                   LeaveSr
     c                   endif
     *
     c                   eval      $esegui=*off
     *
     C                   endsr
     *****************************************************************
     * sCalcQtaRes -  calc.qta da scaricare
     *****************************************************************
     c     sCalcQtaRes   begsr
     *
 "   * Calcolo se ci sono quantit{ da scaricare per la fase
 "   * che ci arriva da CSIS
 "   C     1             Do        16            $X
 "   C                   If        RLNS($x) > *ZEROS

     c                   if        fcolib =  'Y'  and fvirib<>'Y'
     c                   if        utfbin='Y'
 "   C                   Select
 "   C                   When      ICR($x)  >= RLNS($x)
 "   C                   Eval      ICR$($x)   = RLNS($x)
 "   C                   Eval      $QTATOTALE = $QTATOTALE - RLNS($x)
 "   C                   Eval      RLNS($x)   = *Zeros
 "   C                   Eval      $Trovato   = *ON
 "   C                   When      ICR($x)  <  RLNS($x) AND ICR($x) > *ZEROS
 "   C                   Eval      ICR$($x)   = ICR($x)
 "   C                   Eval      $QTATOTALE = $QTATOTALE - ICR($x)
 "   C                   Eval      RLNS($x)   = RLNS($X) - ICR($x)
 "   C                   Eval      $Trovato   = *ON
 "   C                   EndSl
 "
 "    *Scrive qta residue da aggiornare su file MGDIF:
     C                   Eval      LTQ$($x)=LTQ$($x) - ICR$($x)
      *
     C                   else
      *
 "   C                   Select
 "   C                   When      ICZ($x)  >= RLNS($x)
 "   C                   Eval      ICZ$($x)   = RLNS($x)
 "   C                   Eval      $QTATOTALE = $QTATOTALE - RLNS($x)
 "   C                   Eval      RLNS($x)   = *Zeros
 "   C                   Eval      $Trovato   = *ON
 "   C                   When      ICZ($x)  <  RLNS($x) AND ICZ($x) > *ZEROS
 "   C                   Eval      ICZ$($x)   = ICZ($x)
 "   C                   Eval      $QTATOTALE = $QTATOTALE - ICZ($x)
 "   C                   Eval      RLNS($x)   = RLNS($X) - ICZ($x)
 "   C                   Eval      $Trovato   = *ON
 "   C                   EndSl
 "
 "    *Scrive qta residue da aggiornare su file MGDIF:
     C                   Eval      LTQ$($x)=LTQ$($x) - ICZ$($x)
     C                   ENDIF
      *
     c                   else
 "   C                   Select
 "   C                   When      ICE($x)  >= RLNS($x)
 "   C                   Eval      ICE$($x)   = RLNS($x)
 "   C                   Eval      $QTATOTALE = $QTATOTALE - RLNS($x)
 "   C                   Eval      RLNS($x)   = *Zeros
 "   C                   Eval      $Trovato   = *ON
 "   C                   When      ICE($x)  <  RLNS($x) AND ICE($x) > *ZEROS
 "   C                   Eval      ICE$($x)   = ICE($x)
 "   C                   Eval      $QTATOTALE = $QTATOTALE - ICE($x)
 "   C                   Eval      RLNS($x)   = RLNS($X) - ICE($x)
 "   C                   Eval      $Trovato   = *ON
 "   C                   EndSl
 "
 "    *Scrive qta residue da aggiornare su file MGDIF:
     C                   Eval      LTQ$($x)=LTQ$($x) - ICE$($x)
     C                   ENDIF
      *
     C                   EVAL      $CFPD=ICCFPD
     C                   EVAL      $CFBP=ICCFBP
     C                   EVAL      $GRPF=ICGRPF
     C                   EVAL      $FCOL=FCOLIB
 "
 "   C                   EndIf
 "   C                   EndDo
 "
     C                   endsr
     *****************************************************************
     * sCallMG400
     *****************************************************************
     c     sCallMG400    begsr
     *
 "   C                   Clear                   zmg400
 "   C                   Eval      $40ficoaa = 'Y'
 "   C                   Eval      $40gecoaa = 'N'
 "   C                   Eval      $40primaa = 'N'
 "
BET5  * Verifico se far elaborare la chiusura del c/Lavoro
 "    * N.B.: Per le pdc accoppiate viene aperta solo un ariga di C/lavoro con la CPP accoppiata
 "   C                   If        NOT $PrimaCPP
 "   C                   Eval      $40noclaa = 'Y'                              non elaboro c/lavoro
BET5 C                   EndIf
 "
 "   C                   Eval      $40NDOCAA = S1NDOC
 "   C                   Eval      $40DDOCAA = S1DATA

      * Compilo l'udb solo se ho un collaudo; gi{ verificato in precedenza su tabella AWB
      * QUindi se il magazznio non } valorizzato } xk} non ho trovato l'elemento in tabella.
     C                   If        CMAGLK <> *blanks
 "   C                   Eval      $40UDBCAA = APCUDB
     C                   Endif
 "
 "   C                   Eval      $40CSTGAA = icCSTG
 "   C                   Eval      $40CUDBAA = icCUDB
 "   C                   Eval      $40CCLIAA = icCCLI
 "   C                   Eval      $40NORDAA = icNORD
 "   C                   Eval      $40NRIGAA = icNRIG
 "   C                   Eval      $40SRIGAA = icSRIG
 "   C                   Eval      $40NOPRAA = icNOPR
 "   C                   Eval      $40ROPRAA = icROPR
 "   C                   Eval      $40SRPRAA = icSRPR
 "   C                   Eval      $40CCPPAA = $$CCPP
 "   C                   Eval      $40cfbpaa = iccfbp
 "   C                   Eval      $40cfpdaa = iccfpd
 "   C                   Eval      $40pgmcaa = @pgm
 "   C                   Eval      $40ccpoaa = $$ccpp
 "   C                   Eval      $40grpfaa = icgrpf
BET5  * Passo categoria prodotto accoppiamento
BET5 C                   Eval      $40ccpaaa = E6ccpa
 "
 "   C                   Eval      $40clavaa = ctilib
 "
     c                   if        fcolib =  'Y'  and fvirib<>'Y'
     c                   if        utfbin='Y'
 "   C                   Eval      $40qtataa = %XFOOT(icr$)
 "   C                   Eval      $40qta01  = icr$(01)
 "   C                   Eval      $40qta02  = icr$(02)
 "   C                   Eval      $40qta03  = icr$(03)
 "   C                   Eval      $40qta04  = icr$(04)
 "   C                   Eval      $40qta05  = icr$(05)
 "   C                   Eval      $40qta06  = icr$(06)
 "   C                   Eval      $40qta07  = icr$(07)
 "   C                   Eval      $40qta08  = icr$(08)
 "   C                   Eval      $40qta09  = icr$(09)
 "   C                   Eval      $40qta10  = icr$(10)
 "   C                   Eval      $40qta11  = icr$(11)
 "   C                   Eval      $40qta12  = icr$(12)
 "   C                   Eval      $40qta13  = icr$(13)
 "   C                   Eval      $40qta14  = icr$(14)
 "   C                   Eval      $40qta15  = icr$(15)
 "   C                   Eval      $40qta16  = icr$(16)
     c                   else
 "   C                   Eval      $40qtataa = %XFOOT(icz$)
 "   C                   Eval      $40qta01  = icz$(01)
 "   C                   Eval      $40qta02  = icz$(02)
 "   C                   Eval      $40qta03  = icz$(03)
 "   C                   Eval      $40qta04  = icz$(04)
 "   C                   Eval      $40qta05  = icz$(05)
 "   C                   Eval      $40qta06  = icz$(06)
 "   C                   Eval      $40qta07  = icz$(07)
 "   C                   Eval      $40qta08  = icz$(08)
 "   C                   Eval      $40qta09  = icz$(09)
 "   C                   Eval      $40qta10  = icz$(10)
 "   C                   Eval      $40qta11  = icz$(11)
 "   C                   Eval      $40qta12  = icz$(12)
 "   C                   Eval      $40qta13  = icz$(13)
 "   C                   Eval      $40qta14  = icz$(14)
 "   C                   Eval      $40qta15  = icz$(15)
 "   C                   Eval      $40qta16  = icz$(16)
     c                   endif
      *
     c                   else
      *
 "   C                   Eval      $40qtataa = %XFOOT(ice$)
 "   C                   Eval      $40qta01  = ice$(01)
 "   C                   Eval      $40qta02  = ice$(02)
 "   C                   Eval      $40qta03  = ice$(03)
 "   C                   Eval      $40qta04  = ice$(04)
 "   C                   Eval      $40qta05  = ice$(05)
 "   C                   Eval      $40qta06  = ice$(06)
 "   C                   Eval      $40qta07  = ice$(07)
 "   C                   Eval      $40qta08  = ice$(08)
 "   C                   Eval      $40qta09  = ice$(09)
 "   C                   Eval      $40qta10  = ice$(10)
 "   C                   Eval      $40qta11  = ice$(11)
 "   C                   Eval      $40qta12  = ice$(12)
 "   C                   Eval      $40qta13  = ice$(13)
 "   C                   Eval      $40qta14  = ice$(14)
 "   C                   Eval      $40qta15  = ice$(15)
 "   C                   Eval      $40qta16  = ice$(16)
     C                   endif
     c                   if        fcolib =  'Y'  and fvirib='N' and utfbin='Y'
     C                   eval      $40serien='Y'
     C                   endif
     c                   if        fcolib =  'Y'  and fvirib='N' and utfbin='N'
     C                   eval      $40serien='E'
     C                   endif
     c                   if        fcolib <> 'Y'  and s1stnw='RIC'
     C                   eval      $40serien='F'
     C                   endif
 "    *
     C     Kfapes        chain     fapes01k
     C                   Eval      $40cforaa = utcfor
     C                   Eval      $40udbfaa = utudbg
 "    *
     C                   IF        %found(fapes01k) and utcfor <> *blanks
 "   C                   Eval      $KPJBU = KPJBU
 "   C                   Eval      kpjbu  = zmg400
 "   C                   Call      'MG401'
 "   C                   Parm                    kpjba
 "   C                   Eval      zmg400 = kpjbu
 "   C                   Eval      kpjbu  = $KPJBU
     C                   Else
     C                   Eval      $40Critaa = 'ER'
     C                   Eval      $40cmsgaa = 'SNS8099'
     C                   Endif
 "
 "   C                   If        $40Critaa <> *Blanks
 "   C                   eval      $errore = 'E'
 "   C                   clear                   zbferr
 "   C                   eval      uborab = S1ubor
 "   C                   eval      tdorab = S1tdor
 "   C                   eval      pdorab = S1pdor
 "   C                   eval      ndorab = S1ndor
 "   C                   eval      ubptab = S1ubpt
 "   C                   eval      protab = S1prot
 "   C                   eval      nrorab = S1nror
 "   C                   eval      msgeab = $40cmsgaa
 "   C                   eval      ccpaab = e6ccpa
 "
 "   C                   eval      CSTGAB = e6CSTG
 "
 "   C                   eval      CSTGAB = e6CSTG
 "   C                   eval      CUDBAB = e6CUDB
 "   C                   eval      CCLIAB = e6CCLI
 "   C                   eval      NORDAB = e6NORD
 "   C                   eval      NRIGAB = e6NRIG
 "   C                   eval      SRIGAB = e6SRIG
 "   C                   eval      NOPRAB = e6NOPR
 "   C                   eval      ROPRAB = e6ROPR
 "   C                   eval      SRPRAB = e6SRPR
 "   C                   eval      CCPPAB = e6ccpa
 "   C                   eval      CFAMAB = e6cfam
 "
 "   C                   exsr      Errore
 "   C                   MOVEL     'RB'          $40critaa
 "   C                   Eval      $KPJBU = KPJBU
 "   C                   Eval      kpjbu  = zmg400
 "   C                   Call      'MG401'
 "   C                   Parm                    kpjba
 "   C                   Eval      zmg400 = kpjbu
 "   C                   Eval      kpjbu  = $KPJBU
 "   C                   EndIf
 "   *
     C                   endsr
      ********************************************************************************
      *  Sr_main:Elimino taglie se causale lo prevede su ORCRI / ORPRA               *
      ********************************************************************************
     c     Sr_amain      begsr
      *   ORCRI
 "   C     Kriga         Chain     orcri28k
 "   C                   If        not %found
 "   C     Krigap        Chain     orcri28k
 "   C                   EndIf
      *- Causale variaz.
     C                   CLEAR                   z@cvar
     c                   movel     'Z9'          $cvar
     c                   Movel(P)  'VAR'         $ctab
     c                   Movel(P)  $cvar         $cele
     c                   Exsr      Ctl_decotab
     c                   If        @DLI3 = '1'
     c                   eval      $errore = 'E'
     c                   clear                   zbferr
     c                   eval      uborab = S1ubor
     c                   eval      tdorab = S1tdor
     c                   eval      pdorab = S1pdor
     c                   eval      ndorab = S1ndor
     c                   eval      ubptab = S1ubpt
     c                   eval      protab = S1prot
     c                   eval      nrorab = S1nror
     c                   eval      cvarab = $cvar
     c                   eval      msgeab = 'SNS3498'
     c                   exsr      Errore
     c                   exsr      Errore
     c                   leavesr
     c                   Else
     c                   Movel(P)  @dli2         z@cvar
     c                   endif
      *  Testo flag abilita registrazione variazione
     C                   IF        flvrb5='Y'
      * Salvataggio Campi ORCRIAUF per Scrittura ORCVRFRF
     C                   CLEAR                   ZVE500
     C                   MOVE      AUCUDB        CUDBGF
     C                   MOVE      AUCSTG        CSTGGF
     C                   MOVE      AUCCLI        CCLIGF
     C                   Z-ADD     AUNORD        NORDGF
     C                   Z-ADD     AUNRIG        NRIGGF
     C                   Z-ADD     AUSRIG        SRIGGF
     C                   MOVE      AUCTES        CTESGF
     C                   MOVE      AUCOL3        CO3TGF
     C                   MOVE      AUCTEC        CTECGF
     C                   MOVE      AUCOLC        COLCGF
     C                   MOVE      AUCMOD        CMODGF
     C                   MOVE      AUCFPT        CFPTGF
     C                   MOVE      AUCFPM        CFPMGF
     C                   MOVE      AUCFOD        CFODGF
     C                   MOVE      AUCOLF        CO3FGF
     C                   MOVE      AUCBOT        CBOTGF
     C                   MOVE      AUCOLB        CO3BGF
     C                   MOVE      AUCVFP        CVFPGF
     C                   MOVE      AUCIMP        CIMPGF
     C                   MOVE      AUTASO        TASOGF
     C                   MOVE      AUTSCT        TSCTGF
     C                   MOVE      AUCCCT        CCCTGF
     C                   MOVE      AUCCIP        CCIPGF
     C                   MOVE      AUCSER        CSERGF
     C                   MOVE      AUCCMP        CCMPGF
     C                   MOVE      AUSTAT        STATGF
     C                   MOVE      AUCDRO        CDROGF
     C                   MOVE      AUCSTA        CSTAGF
     C                   Z-ADD     AUPSCR        PSCRGF
     C                   Z-ADD     AUVSCR        VSCRGF
     C                   Z-ADD     AUVSRE        VSREGF
     C                   MOVE      @PGM          PGMVGF
     C                   Z-ADD     AUFABR        FABRGF
     C                   MOVE      AUCMOS        CMOSGF
     C                   MOVE      AUTESO        TESOGF
     C                   MOVE      AUCOLO        CO3SGF
     C                   MOVE      AUCTE2        CTE2GF
     C                   MOVE      AUCLT2        CO32GF
     C                   Z-ADD     AUNFB2        NFB2GF
     C                   ENDIF
      *
      * Se la causale di variazione ammette l'aggiornamento delle qta
     C                   if        flqab5 = 'Y'
     C                   Exsr      Sr_aggqta
     C                   endif
      *
      * Se flag lo prevede scrivo la variazione
     C                   if        flvrb5 = 'Y'
     c                   exsr      Sr_aggcvr
     C                   endif
      *   Aggiorno preannullamento
     C                   exsr      Sr_aggcpa
     c
     c
     c                   endsr
      ********************************************************************************
      *  Sr_aggqta -  Routine aggiornamento quantita  x 2[scelta                     *
      ********************************************************************************
     c     Sr_aggqta     begsr
     c
      * Forzo codice variazione
     C                   eval      aucvar = $cvar
     c
     C                   Eval      aua01 = aua01 - RLNC(01)
     C                   If        Aua01 < 0
     C                   Eval      Aua01 = 0
     C                   Endif

     C                   Eval      aua02 = aua02 - RLNC(02)
     C                   If        Aua02 < 0
     C                   Eval      Aua02 = 0
     C                   Endif

     C                   Eval      aua03 = aua03 - RLNC(03)
     C                   If        Aua03 < 0
     C                   Eval      Aua03 = 0
     C                   Endif

     C                   Eval      aua04 = aua04 - RLNC(04)
     C                   If        Aua04 < 0
     C                   Eval      Aua04 = 0
     C                   Endif

     C                   Eval      aua05 = aua05 - RLNC(05)
     C                   If        Aua05 < 0
     C                   Eval      Aua05 = 0
     C                   Endif

     C                   Eval      aua06 = aua06 - RLNC(06)
     C                   If        Aua06 < 0
     C                   Eval      Aua06 = 0
     C                   Endif

     C                   Eval      aua07 = aua07 - RLNC(07)
     C                   If        Aua07 < 0
     C                   Eval      Aua07 = 0
     C                   Endif

     C                   Eval      aua08 = aua08 - RLNC(08)
     C                   If        Aua08 < 0
     C                   Eval      Aua08 = 0
     C                   Endif

     C                   Eval      aua09 = aua09 - RLNC(09)
     C                   If        Aua09 < 0
     C                   Eval      Aua09 = 0
     C                   Endif

     C                   Eval      aua10 = aua10 - RLNC(10)
     C                   If        Aua10 < 0
     C                   Eval      Aua10 = 0
     C                   Endif

     C                   Eval      aua11 = aua11 - RLNC(11)
     C                   If        Aua11 < 0
     C                   Eval      Aua11 = 0
     C                   Endif

     C                   Eval      aua12 = aua12 - RLNC(12)
     C                   If        Aua12 < 0
     C                   Eval      Aua12 = 0
     C                   Endif

     C                   Eval      aua13 = aua13 - RLNC(13)
     C                   If        Aua13 < 0
     C                   Eval      Aua13 = 0
     C                   Endif

     C                   Eval      aua14 = aua14 - RLNC(14)
     C                   If        Aua14 < 0
     C                   Eval      Aua14 = 0
     C                   Endif

     C                   Eval      aua15 = aua15 - RLNC(15)
     C                   If        Aua15 < 0
     C                   Eval      Aua15 = 0
     C                   Endif

     C                   Eval      aua16 = aua16 - RLNC(16)
     C                   If        Aua16 < 0
     C                   Eval      Aua16 = 0
     C                   Endif

     C                   Eval      aucqta = aua01+aua02+aua03+aua04+
     C                                      aua05+aua06+aua07+aua08+
     C                                      aua09+aua10+aua11+aua12+
     C                                      aua13+aua14+aua15+aua16
     C*                  Eval      aucqta = aucqta - S1TOQN
     C                   If        Aucqta < 0
     C                   Eval      Aucqta = 0
     C                   Endif

     *  se e' gestito a bulk nn decurto in quanto gli avanzamneti non vengono  scritti
     c                   if        aucblk=*blank
     C                   Eval      aum01 = aum01 - RLNC(01)
     C                   If        Aum01 < 0
     C                   Eval      Aum01 = 0
     C                   Endif

     C                   Eval      aum02 = aum02 - RLNC(02)
     C                   If        Aum02 < 0
     C                   Eval      Aum02 = 0
     C                   Endif

     C                   Eval      aum03 = aum03 - RLNC(03)
     C                   If        Aum03 < 0
     C                   Eval      Aum03 = 0
     C                   Endif

     C                   Eval      aum04 = aum04 - RLNC(04)
     C                   If        Aum04 < 0
     C                   Eval      Aum04 = 0
     C                   Endif

     C                   Eval      aum05 = aum05 - RLNC(05)
     C                   If        Aum05 < 0
     C                   Eval      Aum05 = 0
     C                   Endif

     C                   Eval      aum06 = aum06 - RLNC(06)
     C                   If        Aum06 < 0
     C                   Eval      Aum06 = 0
     C                   Endif

     C                   Eval      aum07 = aum07 - RLNC(07)
     C                   If        Aum07 < 0
     C                   Eval      Aum07 = 0
     C                   Endif

     C                   Eval      aum08 = aum08 - RLNC(08)
     C                   If        Aum08 < 0
     C                   Eval      Aum08 = 0
     C                   Endif

     C                   Eval      aum09 = aum09 - RLNC(09)
     C                   If        Aum09 < 0
     C                   Eval      Aum09 = 0
     C                   Endif

     C                   Eval      aum10 = aum10 - RLNC(10)
     C                   If        Aum10 < 0
     C                   Eval      Aum10 = 0
     C                   Endif

     C                   Eval      aum11 = aum11 - RLNC(11)
     C                   If        Aum11 < 0
     C                   Eval      Aum11 = 0
     C                   Endif

     C                   Eval      aum12 = aum12 - RLNC(12)
     C                   If        Aum12 < 0
     C                   Eval      Aum12 = 0
     C                   Endif

     C                   Eval      aum13 = aum13 - RLNC(13)
     C                   If        Aum13 < 0
     C                   Eval      Aum13 = 0
     C                   Endif

     C                   Eval      aum14 = aum14 - RLNC(14)
     C                   If        Aum14 < 0
     C                   Eval      Aum14 = 0
     C                   Endif

     C                   Eval      aum15 = aum15 - RLNC(15)
     c                   If        Aum15 < 0
     C                   Eval      Aum15 = 0
     C                   Endif

     C                   Eval      aum16 = aum16 - RLNC(16)
     c                   If        Aum16 < 0
     C                   Eval      Aum16 = 0
     C                   Endif

     C                   Eval      aucqtm = aum01+aum02+aum03+aum04+
     C                                      aum05+aum06+aum07+aum08+
     C                                      aum09+aum10+aum11+aum12+
     C                                      aum13+aum14+aum15+aum16
     c                   If        Aucqtm < 0
     C                   Eval      Aucqtm = 0
     C                   Endif
      *
     C                   Endif
     C
     C                   If        Aucqta<>0 and
     C                             Aucqtm>=Aucqta
     C                   eval      austat = 'RM'
     C                   eval      austan = '205'
     C                   eval      audtma = S1DATA
     C                   endif

     *
     * Se la quantita risulta a zero, annullo la riga
     C                   if        aucqta = *zeros
     * Riprendo causale annullamento da tab. VAR
     C                   eval      austat = STATB5
     * Verifico gia' esistenza storico
     C     statb5        chain     evean01k
     C                   if        %found
     C                   eval      austan = cgstan
     C                   endif
     *
     C                   eval      audtst = *date
     C                   eval      audtan = *date
     C                   endif
     *
     C                   exsr      cabfic
     C                   update    rorcri
     *
     *   ORPRA
     C     KorpraSNS     Setll     orpra01k
     C                   Do        *hival
     C     KorpraSNS     Reade     orpra01k
     *
     C                   If        (%Eof(orpra01k))
     C                   Leave
     C                   EndIf
      *
     C     e6ccpa        chain     cpman01k
     c                   IF        %found(cpman01k)
     c                             and acfgcm = 'Y'
     c                   iter
     c                   endif
     *
     C                   Eval      e6q01 = e6q01 - RLNC(01)
     c                   If        e6q01 < 0
     C                   Eval      e6q01 = 0
     C                   Endif

     C                   Eval      e6q02 = e6q02 - RLNC(02)
     c                   If        e6q02 < 0
     C                   Eval      e6q02 = 0
     C                   Endif

     C                   Eval      e6q03 = e6q03 - RLNC(03)
     c                   If        e6q03 < 0
     C                   Eval      e6q03 = 0
     C                   Endif

     C                   Eval      e6q04 = e6q04 - RLNC(04)
     c                   If        e6q04 < 0
     C                   Eval      e6q04 = 0
     C                   Endif

     C                   Eval      e6q05 = e6q05 - RLNC(05)
     c                   If        e6q05 < 0
     C                   Eval      e6q05 = 0
     C                   Endif

     C                   Eval      e6q06 = e6q06 - RLNC(06)
     c                   If        e6q06 < 0
     C                   Eval      e6q06 = 0
     C                   Endif

     C                   Eval      e6q07 = e6q07 - RLNC(07)
     c                   If        e6q07 < 0
     C                   Eval      e6q07 = 0
     C                   Endif

     C                   Eval      e6q08 = e6q08 - RLNC(08)
     c                   If        e6q08 < 0
     C                   Eval      e6q08 = 0
     C                   Endif

     C                   Eval      e6q09 = e6q09 - RLNC(09)
     c                   If        e6q09 < 0
     C                   Eval      e6q09 = 0
     C                   Endif

     C                   Eval      e6q10 = e6q10 - RLNC(10)
     c                   If        e6q10 < 0
     C                   Eval      e6q10 = 0
     C                   Endif

     C                   Eval      e6q11 = e6q11 - RLNC(11)
     c                   If        e6q11 < 0
     C                   Eval      e6q11 = 0
     C                   Endif

     C                   Eval      e6q12 = e6q12 - RLNC(12)
     c                   If        e6q12 < 0
     C                   Eval      e6q12 = 0
     C                   Endif

     C                   Eval      e6q13 = e6q13 - RLNC(13)
     c                   If        e6q13 < 0
     C                   Eval      e6q13 = 0
     C                   Endif

     C                   Eval      e6q14 = e6q14 - RLNC(14)
     c                   If        e6q14 < 0
     C                   Eval      e6q14 = 0
     C                   Endif

     C                   Eval      e6q15 = e6q15 - RLNC(15)
     c                   If        e6q15 < 0
     C                   Eval      e6q15 = 0
     C                   Endif

     C                   Eval      e6q16 = e6q16 - RLNC(16)
     c                   If        e6q16 < 0
     C                   Eval      e6q16 = 0
     C                   Endif

     C                   Eval      e6qlan = e6q01+e6q02+e6q03+e6q04+
     C                                      e6q05+e6q06+e6q07+e6q08+
     C                                      e6q09+e6q10+e6q11+e6q12+
     C                                      e6q13+e6q14+e6q15+e6q16
     C*                  Eval      e6qlan = e6qlan - s1toqn
     c                   If        e6qlan < 0
     C                   Eval      e6qlan = 0
     C                   Endif

     *
     * Se la quantita risulta a zero, annullo la riga
     C                   if        e6qlan = *zeros
     * Riprendo causale annullamento da tab. VAR
     C                   eval      e6stat = STATB5
     * Recupero stato numerico
     C     statb5        chain     evean01k
     C                   if        %found
     C                   eval      e6stan = cgstan
     C                   endif
     *
     C                   endif
      *
     C                   if        austat='RM'
     C                   eval      e6stat = 'SD'
     C                   eval      e6stan = '231'
     C                   Endif
      *
     C                   exsr      cabfic
     *
     C                   update    orpra01
     c                   Enddo
     *
     c                   endsr
      *----------------------------------------------------------------
      *    Sr_aggcvr - Routine di valorizzazione campi di default
      *----------------------------------------------------------------
     C     Sr_aggcvr     BEGSR
     C                   MOVE      $cvar         CVARGF
     c                   Eval      varigf = S1TOQN  * (-1)
     c                   eval      GFV01= RLNC(1)  * (-1)
     c                   eval      GFV02= RLNC(2)  * (-1)
     c                   eval      GFV03= RLNC(3)  * (-1)
     c                   eval      GFV04= RLNC(4)  * (-1)
     c                   eval      GFV05= RLNC(5)  * (-1)
     c                   eval      GFV06= RLNC(6)  * (-1)
     c                   eval      GFV07= RLNC(7)  * (-1)
     c                   eval      GFV08= RLNC(8)  * (-1)
     c                   eval      GFV09= RLNC(9)  * (-1)
     c                   eval      GFV10= RLNC(10) * (-1)
     c                   eval      GFV11= RLNC(11) * (-1)
     c                   eval      GFV12= RLNC(12) * (-1)
     c                   eval      GFV13= RLNC(13) * (-1)
     c                   eval      GFV14= RLNC(14) * (-1)
     c                   eval      GFV15= RLNC(15) * (-1)
     c                   eval      GFV16= RLNC(16) * (-1)
      *
      ** Salvo KPJBU
     C                   eval      $kpjbu=kpjbu
      ** Muove dati per scrittura record ORCVR
     C                   MOVEL     ZVE500        KPJBU
     C                   CALL      'VE500'
     C                   PARM                    KPJBA
     C                   PARM                    ZVE500
      ** Ripristina KPJBU
     C                   eval      kpjbu=$kpjbu
     C                   clear                   zve500
     c*
     c                   endsr
      *----------------------------------------------------------------
      *    Sr_aggcpa - Aggiornamento ORCPA
      *----------------------------------------------------------------
     C     Sr_aggcpa     BEGSR
      *
     C     Korcp         Klist
     C                   Kfld                    aucudb
     C                   Kfld                    aucstg
     C                   Kfld                    auccli
     C                   Kfld                    aunord
     C                   Kfld                    aunrig
     C                   Kfld                    ausrig
     C                   Kfld                    $cvar
     C                   Kfld                    mgprog
      *
     C     Korcp1        Klist
     C                   Kfld                    aucudb
     C                   Kfld                    aucstg
     C                   Kfld                    auccli
     C                   Kfld                    aunord
     C                   Kfld                    aunrig
     C                   Kfld                    ausrig
     C                   Kfld                    $cvar
     c
      *
     C                   move      *all'9'       mgprog
     C     korcp         setgt     orcpa01k
     C     korcp1        readpe    orcpa01k
     C                   if        %eof(orcpa01k)
     C                   z-add     *zeros        $prog
     C                   else
     C                   eval      $prog=mgprog+1
     C                   endif
      *
     C                   clear                   rorcpa
      *
     C                   EVAL      MGPROG = $PROG
     C                   EVAL      A003MG = ##CUDB
     C                   EVAL      A004MG = ##CUDB
     C                   EVAL      A005MG = ##CUDB
     C                   EVAL      MGCUDB = AUCUDB
     C                   EVAL      MGCSTG = AUCSTG
     C                   EVAL      MGCCLI = AUCCLI
     C                   EVAL      MGNORD = AUNORD
     C                   EVAL      MGNRIG = AUNRIG
     C                   EVAL      MGSRIG = AUSRIG
     C                   EVAL      MGCVAR = AUCVAR
      *  Imposto i capi per taglia
     C                   EVAL      MGC01 = rlnc(1)
     C                   EVAL      MGC02 = rlnc(2)
     C                   EVAL      MGC03 = rlnc(3)
     C                   EVAL      MGC04 = rlnc(4)
     C                   EVAL      MGC05 = rlnc(5)
     C                   EVAL      MGC06 = rlnc(6)
     C                   EVAL      MGC07 = rlnc(7)
     C                   EVAL      MGC08 = rlnc(8)
     C                   EVAL      MGC09 = rlnc(9)
     C                   EVAL      MGC10 = rlnc(10)
     C                   EVAL      MGC11 = rlnc(11)
     C                   EVAL      MGC12 = rlnc(12)
     C                   EVAL      MGC13 = rlnc(13)
     C                   EVAL      MGC14 = rlnc(14)
     C                   EVAL      MGC15 = rlnc(15)
     C                   EVAL      MGC16 = rlnc(16)
      *
     C                   EVAL      MGCQTA = S1TOQN
      *
     C                   eval      MGDATA = *date
     C                   MOVEL     'Y'           MGFELA
     C                   MOVEL     @USER         MGUSER
      *
     C                   MOVEL     @USER         MGUSRV
     C                   eval      MGDATV = *date
      *
     C                   EXSR      CABFIC
     C                   WRITE     RORCPA
      *
     c                   Endsr

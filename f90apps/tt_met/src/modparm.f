      module parm 
      real :: wshd_sw, wshd_snob, wshd_pndfr, wshd_pndv, wshd_pndsed
      real :: wshd_wetfr, wshd_resfr, wshd_resha, wshd_pndha, percop
      real :: wshd_fminp, wshd_ftotn, wshd_fnh3, wshd_fno3, wshd_forgn
      real :: wshd_forgp, wshd_ftotp, wshd_yldn, wshd_yldp, wshd_fixn
      real :: wshd_pup, wshd_wstrs, wshd_nstrs, wshd_pstrs, wshd_tstrs
      real :: wshd_hmn, wshd_rwn, wshd_hmp, wshd_rmn, wshd_dnit, ffcb
      real :: wshd_rmp, wshd_voln, wshd_nitn, wshd_pas, wshd_pal, wdpq
      real :: wshd_plch, wshd_raino3, ressedc, basno3f, basorgnf, wof_p
      real :: basminpf, basorgpf, sftmp, smtmp, smfmx, smfmn, timp, wgpq
      real :: wshd_resv, wshd_ressed, basno3i, basorgni, basminpi, wdlpq
      real :: basorgpi, peakr, pndsedin, sw_excess, albday, wglpq, wdps
      real :: tloss, inflpcp, snomlt, snofall, fixn, qtile, crk, latlyr
      real :: sedrch, fertn, sol_rd, cfertn, cfertp, sepday, bioday
      real :: fertp, grazn, grazp, soxy, qdfr, sdti, rtwtr, ressa, wgps
      real :: rttime, rchdep, rtevp, rttlc, da_km, resflwi, wdlps, wglps
      real :: resflwo, respcp, resev, ressep,ressedi,ressedo,dtot,wdprch
      real :: nperco, pperco, rsdco, phoskd, voltot, volcrmin, msk_x
      real :: uno3d, canev, usle, rcn, surlag, bactkdq, precipday, wdpf
      real :: thbact, wpq20, wlpq20, wps20, wlps20, bactrop, bactsedp
      real :: bactlchp, bactlchlp, enratio, wetpcp, pndpcp, wetsep, wgpf
      real :: pndsep, wetev, pndev, pndsedo, wetsedo, pndflwi, wetflwi
      real :: pndflwo, wetflwo, wetsedi, da_ha, twlwet, twlpnd, vpd
      real :: bactrolp, bactsedlp, evrch, evlai, pet_day, ep_day, wdlpf
      real :: snoev, sno3up, adj_pkr, n_updis, p_updis, nactfr, reactw
      real :: sdiegropq, sdiegrolpq, sdiegrops, sdiegrolps, es_day
      real :: sbactrop, sbactrolp, sbactsedp, sbactsedlp, ep_max, wof_lp
      real :: sbactlchp, sbactlchlp, psp, rchwtr, resuspst, setlpst
      real :: bsprev, bssprev, spadyo, spadyev, spadysp, spadyrfv
      real :: qday, usle_ei, al5, pndsedc, no3pcp, rcharea, volatpst
      real :: wetsedc, uobw, ubw, uobn, uobp, prf, respesti, wglpf
      real :: snocovmx, snocov1, snocov2, rexp, rcor, lyrtile, lyrtilex
      real :: ai0, ai1, ai2, ai3, ai4, ai5, ai6, rhoq, tfact, sno50cov
      real :: mumax, lambda0, lambda1, lambda2, k_l, k_n, k_p, p_n
      real :: rnum1, autop, auton, etday, hmntl, rwntl, hmptl, rmn2tl
      real :: rmptl, wdntl, cmn, rmp1tl, roctl, gwseep, revapday, reswtr
      real :: bury, difus, reactb, solpesto, petmeas, wdlprch, wdpres
      real :: sorpesto, spcon, spexp, solpesti, sorpesti, wdlpres
      real :: snoprev, swprev, shallstp, deepstp, msk_co1, msk_co2
      real :: ressolpo, resorgno, resorgpo, resno3o, reschlao, resno2o
      real :: resnh3o, qdbank, potpcpmm, potevmm, potsepmm, potflwo
      real :: potsedo, pest_sol, trnsrch, wp20p_plt, bactminp, bactminlp
      real :: wp20lp_plt, cncoef, cdn, sdnco, bact_swf, bactmx, bactmin
      real :: chla_subco, tb_adj, cn_froz, dorm_hr
      real :: depimp_bsn, ddrain_bsn, tdrain_bsn, gdrain_bsn
      integer :: mrg, mch, mcr, mpdb, mcrdb, mfdb, mhru, mhyd, mfcst
      integer :: mnr, myr, mcut, mgr, msubo, mstep, mrcho, isubwq, ffcst
      integer :: nhru, isproj, mo, nbyr, immo, nrch, nres, irte, i_mo
      integer :: icode, ihout, inum1, inum2, inum3, inum4, wndsim, ihru
      integer :: nrgage, ntgage, nrgfil, ntgfil, nrtot, nttot, mrech
      integer :: lao, igropt, npmx, irtpest, curyr, tmpsim, icrk, iihru
      integer :: mtil, mvaro, mrecd, idist, mudb, mrecm, mrecc, iclb
      integer :: mrecy, ipet, nyskip, ideg, ievent, slrsim
      integer :: id1, idaf, idal, leapyr, mo_chk, rhsim, mstdo
      integer :: ifirsts, ifirsth, ifirstw, nstot, nhtot, nwtot, icst
      integer :: ilog, i, iyr, itotr, iwq, iskip, scenario, ifirstpet
      integer :: itotb, itots, iprp, pcpsim, itoth, nd_30
      integer :: iscen, fcstyr, fcstday, fcstcycles, iprs, subtot, ogen
      integer :: msub, mhruo, mres, mapp, mpst, mlyr, igen, iprint, iida
      integer :: fcstcnt, icn, ised_det, mtran
! additional variables for PET generation
      integer :: istyr,istdy,npedays,idwgn,ijday
!      real :: elev, wlat      
      real, dimension (:), allocatable :: precip, tmax, tmin, pevtpm
      real, dimension (:), allocatable :: pcpd
! date
      character(len=8) :: date
      character(len=10) :: time
      character(len=5) :: zone
      character(len=80) :: prog
      character(len=13) :: slrfile, wndfile, rhfile, petfile, calfile
      character(len=13) :: atmofile
      integer, dimension (:), allocatable :: ifirstr, idg, ifirsthr
      integer, dimension (:), allocatable :: values, ndays
! output files
      integer, dimension (:), allocatable :: icolb,icolr,icolrsv,icols
      integer, dimension (:), allocatable :: ipdvar,ipdvab,ipdvas,ipdhru
      real, dimension (:), allocatable :: wshddayo,wshdmono,wshdyro
      real, dimension (:), allocatable :: wshdaao,fcstaao
      real, dimension (:,:), allocatable :: wpstdayo,wpstmono,wpstyro
      real, dimension (:,:), allocatable :: wpstaao,rchmono,rchyro
      real, dimension (:,:), allocatable :: rchaao,rchdy,hrumono,hruyro
      real, dimension (:,:), allocatable :: hruaao,submono,subyro,subaao
      real, dimension (:,:), allocatable :: resoutm,resouty,resouta
      real, dimension (:,:), allocatable :: wshd_aamon
      real, dimension (:,:), allocatable :: wtrmon,wtryr,wtraa
      real, dimension (:,:,:), allocatable :: hrupstd,hrupsta,hrupstm
      real, dimension (:,:,:), allocatable :: hrupsty
! mrg = max number of rainfall/temperature gages
      integer, dimension (:), allocatable :: ifirstt,ifirstpcp
      integer, dimension (:), allocatable :: elevp,elevt
! mfcst = max number of forecast regions
      real, dimension (:,:), allocatable :: ftmpstdmn,ftmpmn,ftmpmx
      real, dimension (:,:), allocatable :: ftmpstdmx
      real, dimension (:,:,:), allocatable :: fpr_w,fpcp_stat
! mch = max number of channels
      real, dimension (:), allocatable :: flwin,flwout,bankst,ch_wi,ch_d
      real, dimension (:), allocatable :: drift,rch_dox,rch_bactp
      real, dimension (:), allocatable :: alpha_bnk,alpha_bnke
      real, dimension (:), allocatable :: disolvp,algae,sedst,rchstor
      real, dimension (:), allocatable :: organicn,organicp,chlora
      real, dimension (:), allocatable :: nitraten,nitriten,ch_li,ch_si
      real, dimension (:), allocatable :: ch_cov,ch_di,ch_erod,ch_l2
      real, dimension (:), allocatable :: chpst_conc,chpst_rea,chpst_vol
      real, dimension (:), allocatable :: chpst_koc,chpst_stl,chpst_rsp
      real, dimension (:), allocatable :: chpst_mix,sedpst_conc,ch_wdr
      real, dimension (:), allocatable :: sedpst_rea,sedpst_bry
      real, dimension (:), allocatable :: sedpst_act,rch_cbod,rch_bactlp
      real, dimension (:), allocatable :: chside,rs1,rs2,rs3,rs4,rs5
      real, dimension (:), allocatable :: rs6,rs7,rk1,rk2,rk3,rk4,rk5
      real, dimension (:), allocatable :: rk6,bc1,bc2,bc3,bc4,ammonian
      real, dimension (:), allocatable :: orig_sedpstconc
      real, dimension (:,:), allocatable :: wurch
      integer, dimension (:), allocatable :: icanal
      integer, dimension (:), allocatable :: itb
! msub = max number of subbasins
      real, dimension (:), allocatable :: ch_revap
      real, dimension (:), allocatable :: harg_petco, subfr_nowtr
      real, dimension (:), allocatable :: wcklsp,sub_fr,sub_minp,sub_sw
      real, dimension (:), allocatable :: sub_sumfc,sub_gwno3,sub_gwsolp
      real, dimension (:), allocatable :: sub_km,sub_tc,wlat,sub_pet,co2
      real, dimension (:), allocatable :: welev,sub_orgn,sub_orgp,sub_bd
      real, dimension (:), allocatable :: sub_wtmp,sub_sedpa,sub_sedps
      real, dimension (:), allocatable :: sub_minpa,sub_minps,daylmn
      real, dimension (:), allocatable :: latcos,latsin,phutot
      real, dimension (:), allocatable :: tlaps,plaps,tmp_an,sub_precip
      real, dimension (:), allocatable :: pcpdays, rcn_sub, rammo_sub
      real, dimension (:), allocatable :: sub_snom,sub_qd,sub_sedy
      real, dimension (:), allocatable :: sub_tran,sub_no3,sub_latno3
      real, dimension (:), allocatable :: sub_solp,sub_subp,sub_etday
      real, dimension (:), allocatable :: sub_wyld,sub_surfq,sub_elev
      real, dimension (:), allocatable :: sub_gwq,sub_sep,sub_chl
      real, dimension (:), allocatable :: sub_cbod,sub_dox,sub_solpst
      real, dimension (:), allocatable :: sub_sorpst,sub_yorgn,sub_yorgp
      real, dimension (:), allocatable :: sub_bactp,sub_bactlp,sub_lat
      real, dimension (:), allocatable :: sub_latq, hqd, hqdsave
      real, dimension (:,:), allocatable :: sub_pst,sub_hhqd,sub_hhwtmp
      real, dimension (:,:), allocatable :: rfinc,tmpinc,radinc,huminc
      real, dimension (:,:), allocatable :: wndav,ch_k,elevb,elevb_fr
      real, dimension (:,:), allocatable :: dewpt,ch_w,ch_s,ch_n
      real, dimension (:,:), allocatable :: amp_r,solarav,tmpstdmx
      real, dimension (:,:), allocatable :: tmpstdmn,pcf,tmpmn,tmpmx
      real, dimension (:,:), allocatable :: otmpstdmn,otmpmn,otmpmx
      real, dimension (:,:), allocatable :: otmpstdmx
      real, dimension (:,:), allocatable :: uh
      real, dimension (:,:,:), allocatable :: pr_w,pcp_stat
      real, dimension (:,:,:), allocatable :: opr_w,opcp_stat
      integer, dimension (:), allocatable :: hrutot,hru1,ireg
      integer, dimension (:), allocatable :: isgage,ihgage,iwgage
      integer, dimension (:), allocatable :: irgage,itgage,subgis
      integer, dimension (:), allocatable :: fcst_reg
! mlyr = max number of soil layers
      real, dimension (:,:), allocatable :: sol_aorgn,sol_tmp,sol_fon
      real, dimension (:,:), allocatable :: sol_awc,sol_prk,volcr
      real, dimension (:,:), allocatable :: sol_actp,sol_stap,conv_wt
      real, dimension (:,:), allocatable :: sol_solp,sol_ul,sol_fc,crdep
      real, dimension (:,:), allocatable :: sol_z,sol_up,sol_bd,sol_st
      real, dimension (:,:), allocatable :: flat,sol_nh3,sol_hk,sol_clay
      real, dimension (:,:), allocatable :: sol_orgn,sol_por,sol_wp
      real, dimension (:,:), allocatable :: sol_orgp,sol_hum,sol_wpmm
      real, dimension (:,:), allocatable :: sol_k,sol_cbn,sol_no3
      real, dimension (:,:), allocatable :: sol_rsd,sol_fop
      real, dimension (:,:), allocatable :: orig_solno3,orig_solorgn
      real, dimension (:,:), allocatable :: orig_solsolp,orig_solorgp
      real, dimension (:,:), allocatable :: orig_soltmp,orig_solrsd
      real, dimension (:,:), allocatable :: orig_solfop,orig_solfon
      real, dimension (:,:), allocatable :: orig_solaorgn,orig_solst
      real, dimension (:,:), allocatable :: orig_solactp,orig_solstap
      real, dimension (:,:), allocatable :: orig_volcr
      real, dimension (:,:,:), allocatable :: sol_pst,sol_kp
      real, dimension (:,:,:), allocatable :: orig_solpst
! mres = max number of reservoirs
      real, dimension (:), allocatable :: br1,res_k,lkpst_conc, evrsv
      real, dimension (:), allocatable :: res_evol,res_pvol,res_vol
      real, dimension (:), allocatable :: res_psa,lkpst_rea,lkpst_vol
      real, dimension (:), allocatable :: br2,res_rr,res_sed,lkpst_koc
      real, dimension (:), allocatable :: lkpst_stl,lkpst_rsp,lkpst_mix
      real, dimension (:), allocatable :: lkspst_conc,lkspst_rea
      real, dimension (:), allocatable :: lkspst_bry,lkspst_act,sed_stlr
      real, dimension (:), allocatable :: wurtnf,res_nsed,resdata,chlar
      real, dimension (:), allocatable :: res_orgn,res_orgp,res_no3
      real, dimension (:), allocatable :: res_solp,res_chla,res_seci
      real, dimension (:), allocatable :: res_esa,seccir,res_no2,res_nh3
      real, dimension (:), allocatable :: res_bactp, res_bactlp
      real, dimension (:), allocatable :: orig_resvol,orig_ressed
      real, dimension (:), allocatable :: orig_lkpstconc,orig_lkspstconc
      real, dimension (:), allocatable :: orig_ressolp,orig_resorgp
      real, dimension (:), allocatable :: orig_resno3,orig_resno2
      real, dimension (:), allocatable :: orig_resnh3,orig_resorgn
      real, dimension (:,:), allocatable :: starg,oflowmx,oflowmn
      real, dimension (:,:), allocatable :: psetlr,nsetlr,wuresn
      real, dimension (:,:,:), allocatable :: res_out
      integer, dimension (:), allocatable :: ires1,ires2,res_sub
      integer, dimension (:), allocatable :: iresco,mores,iyres
      integer, dimension (:), allocatable :: iflod1r,iflod2r,ndtargr
! mpdb = max number of pesticides in the database
      real, dimension (:), allocatable :: skoc,ap_ef,decay_f
      real, dimension (:), allocatable :: hlife_f,hlife_s,decay_s
      real, dimension (:), allocatable :: pst_wsol,pst_wof
      integer, dimension (:), allocatable :: nope,pstflg
! mcrdb = maximum number of crops in database
      real, dimension (:), allocatable :: wac21,wac22,cnyld,rsdco_pl
      real, dimension (:), allocatable :: wsyf,leaf1,leaf2,alai_min
      real, dimension (:), allocatable :: t_base,t_opt,hvsti,bio_e
      real, dimension (:), allocatable :: vpd2,gsi,chtmx,wavp,cvm
      real, dimension (:), allocatable :: blai,dlai,rdmx,cpyld,bio_leaf
      real, dimension (:), allocatable :: bio_n1,bio_n2,bio_p1,bio_p2
      real, dimension (:), allocatable :: bmx_trees,ext_coef
      real, dimension (:), allocatable :: air_str
      real, dimension (:,:), allocatable :: pltnfr,pltpfr
      integer, dimension (:), allocatable :: idc, mat_yrs
! mfdb = maximum number of fertilizer in database
      real, dimension (:), allocatable :: forgn,forgp,fminn,bactpdb
      real, dimension (:), allocatable :: fminp,fnh3n,bactlpdb,bactkddb
      character(len=8), dimension (100) :: fertnm
! mudb = maximum number of land types in urban database
      real, dimension (:), allocatable :: fimp,curbden,urbcoef,dirtmx
      real, dimension (:), allocatable :: thalf,tnconc,tpconc,tno3conc
      real, dimension (:), allocatable :: fcimp,urbcn2
! mapp = max number of applications
      real, dimension (:,:,:), allocatable :: sweepeff,frt_kg
      real, dimension (:,:,:), allocatable :: pst_kg,irr_salt
      real, dimension (:,:,:), allocatable :: cnop,phuimp,phusw,irr_amt
      real, dimension (:,:,:), allocatable :: phut,phun,auto_wstr,phucf
      real, dimension (:,:,:), allocatable :: phui,phuai,phuaf,phupst
      real, dimension (:,:,:), allocatable :: cfrt_kg,phuirr,frt_surface
      real, dimension (:,:,:), allocatable :: fr_curb
      real, dimension (:), allocatable :: auto_nstrs,afrt_surface
      real, dimension (:), allocatable :: auto_napp,auto_nyr
      integer, dimension (:,:,:), allocatable :: ifert,iop,iir,ipst
      integer, dimension (:,:,:), allocatable :: iairr,iafer,isweep
      integer, dimension (:,:,:), allocatable :: irelease,ipest
      integer, dimension (:), allocatable :: iafrttyp
      integer, dimension (:,:,:), allocatable :: ifrttyp,idtill
      integer, dimension (:,:,:), allocatable :: icfert,cfrt_id
      integer, dimension (:,:,:), allocatable :: ifrt_freq,fert_days
      integer, dimension (:,:,:), allocatable :: wstrs_id, imp_trig
      integer, dimension (:,:,:), allocatable :: icpest,ipst_freq
      integer, dimension (:,:,:), allocatable :: cpst_kg,cpst_id,phucp
      integer, dimension (:,:,:), allocatable :: pest_days
! mnr = max number years of rotation
      real, dimension (:,:,:), allocatable :: yldkg,bio_trmp
      real, dimension (:,:,:), allocatable :: tnyld,tnylda,phup,lai_init
      real, dimension (:,:,:), allocatable :: bio_init,phuh,phuho,phuk
      real, dimension (:,:,:), allocatable :: phug,bio_eat
      real, dimension (:,:,:), allocatable :: harveff,yldn,manure_kg
      real, dimension (:,:,:), allocatable :: orig_tnylda,orig_phu
      real, dimension (:,:,:), allocatable :: bio_hv,phu_plt,bio_aahv
      real, dimension (:,:,:), allocatable :: hi_targ,bio_targ,hi_ovr
      integer, dimension (:,:,:), allocatable :: ihv,ikill,ihvo,igraz
      integer, dimension (:,:,:), allocatable :: ncrops,iplant,idplt
      integer, dimension (:,:,:), allocatable :: grz_days,manure_id
! mtil = max number tillages in database
      real, dimension (:), allocatable :: effmix,deptil
      character(len=8), dimension (550) :: tillnm
! mhyd = max number of hydrograph nodes
      real, dimension (:), allocatable :: rnum1s,hyd_dakm
      real, dimension (:,:), allocatable :: varoute,shyd
      real, dimension (:,:,:), allocatable :: hhvaroute
      integer, dimension (:), allocatable :: icodes,ihouts,inum1s
      integer, dimension (:), allocatable :: inum2s,inum3s,inum4s
      integer, dimension (:), allocatable :: subed
      character(len=10), dimension (:), allocatable :: recmonps
      character(len=10), dimension (:), allocatable :: reccnstps
! mhru = maximum number of hydrologic response units
      real, dimension (:), allocatable :: pot_fr,pot_tile,pot_vol,potsa
      real, dimension (:), allocatable :: pot_volx,potflwi,potsedi,wfsh
      real, dimension (:), allocatable :: pot_nsed,pot_no3l,newrti,gwno3
      real, dimension (:), allocatable :: pot_sed,pot_no3,fsred,tmpavp
      real, dimension (:), allocatable :: evpot
      real, dimension (:), allocatable :: filterw,sumix,usle_ls,phuacc
      real, dimension (:), allocatable :: esco,epco,slsubbsn,hru_slp
      real, dimension (:), allocatable :: erorgn,erorgp,biomix,pnd_seci
      real, dimension (:), allocatable :: flowmin,divmax,canmx,usle_p
      real, dimension (:), allocatable :: lat_sed,rch_dakm,pnd_no3s,cn1
      real, dimension (:), allocatable :: cn2,lat_ttime,flowfr,sol_zmx
      real, dimension (:), allocatable :: tile_ttime
      real, dimension (:), allocatable :: slsoil,sed_stl,gwminp,sol_cov
      real, dimension (:), allocatable :: yldanu,pnd_solp,pnd_no3,ov_n
      real, dimension (:), allocatable :: driftco,pnd_orgp,pnd_orgn,cn3
      real, dimension (:), allocatable :: sol_sumul,pnd_chla,hru_fr
      real, dimension (:), allocatable :: bio_ms,sol_alb,strsw,hru_km
      real, dimension (:), allocatable :: pnd_fr,pnd_psa,pnd_pvol,pnd_k
      real, dimension (:), allocatable :: pnd_esa,pnd_evol,pnd_vol,yldaa
      real, dimension (:), allocatable :: pnd_sed,pnd_nsed,strsa,dep_imp
      real, dimension (:), allocatable :: evpnd, evwet
      real, dimension (:), allocatable :: wet_fr,wet_nsa,wet_nvol,wet_k
      real, dimension (:), allocatable :: wet_mxsa,wet_mxvol,wet_vol
      real, dimension (:), allocatable :: wet_sed,wet_nsed
      real, dimension (:), allocatable :: smx,sci,bp1,bp2
      real, dimension (:), allocatable :: bw1,bw2,bactpq
      real, dimension (:), allocatable :: bactp_plt,bactlp_plt,cnday
      real, dimension (:), allocatable :: bactlpq,auto_eff,sol_sw,secciw
      real, dimension (:), allocatable :: bactps,bactlps,tmpav,chlaw
      real, dimension (:), allocatable :: subp,sno_hru,hru_ra,wet_orgn
      real, dimension (:), allocatable :: tmx,tmn,rsdin,tmp_hi,tmp_lo
      real, dimension (:), allocatable :: rwt,olai,usle_k,tconc,hru_rmx
      real, dimension (:), allocatable :: anano3,aird,t_ov,sol_sumfc
      real, dimension (:), allocatable :: sol_avpor,usle_mult,wet_orgp
      real, dimension (:), allocatable :: rock,silt,aairr,cht,u10,rhd
      real, dimension (:), allocatable :: shallirr,deepirr,lai_aamx
      real, dimension (:), allocatable :: canstor,ovrlnd,ch_l1,wet_no3
      real, dimension (:), allocatable :: wet_solp,wet_no3s,wet_chla
      real, dimension (:), allocatable :: wet_seci,pnd_no3g,pstsol
      real, dimension (:), allocatable :: gwht,delay,gw_q,pnd_solpg
      real, dimension (:), allocatable :: alpha_bf,alpha_bfe,gw_spyld
      real, dimension (:), allocatable :: gw_delaye,gw_revap,rchrg_dp
      real, dimension (:), allocatable :: revapmn,anion_excl,rchrg
      real, dimension (:), allocatable :: ffc,bio_min,surqsolp
      real, dimension (:), allocatable :: cklsp,deepst,shallst,wet_solpg
      real, dimension (:), allocatable :: wet_no3g,sol_avbd,trapeff
      real, dimension (:), allocatable :: gwqmn,tdrain,pplnt,snotmp
      real, dimension (:), allocatable :: ddrain,gdrain,sol_crk,dayl,brt
      real, dimension (:), allocatable :: twash,rnd2,rnd3,sol_cnsw,doxq
      real, dimension (:), allocatable :: rnd8,rnd9,percn,sol_sumwp
      real, dimension (:), allocatable :: tauton,tautop,cbodu,chl_a,qdr
      real, dimension (:), allocatable :: tfertn,tfertp,tgrazn,tgrazp
      real, dimension (:), allocatable :: latno3,latq,minpgw,no3gw,nplnt
      real, dimension (:), allocatable :: tileq, tileno3
      real, dimension (:), allocatable :: sedminpa,sedminps,sedorgn
      real, dimension (:), allocatable :: sedorgp,sedyld,sepbtm,strsn
      real, dimension (:), allocatable :: strsp,strstmp,surfq,surqno3
      real, dimension (:), allocatable :: tcfrtn,tcfrtp,hru_ha,hru_dafr
      real, dimension (:), allocatable :: phubase,bio_yrms,hvstiadj
      real, dimension (:), allocatable :: laimxfr,laiday,chlap,pnd_psed
      real, dimension (:), allocatable :: wet_psed,seccip,plantn,plt_et
      real, dimension (:), allocatable :: plt_pet,plantp,bio_aams
      real, dimension (:), allocatable :: bio_aamx,lai_yrmx,dormhr
      real, dimension (:), allocatable :: lat_pst
      real, dimension (:), allocatable :: orig_snohru,orig_potvol,fld_fr
      real, dimension (:), allocatable :: orig_alai,orig_bioms,pltfr_n
      real, dimension (:), allocatable :: orig_phuacc,orig_sumix,pltfr_p
      real, dimension (:), allocatable :: orig_shallst,orig_deepst
      real, dimension (:), allocatable :: orig_pndvol,orig_pndsed,rip_fr
      real, dimension (:), allocatable :: orig_pndno3,orig_pndsolp
      real, dimension (:), allocatable :: orig_pndorgn,orig_pndorgp
      real, dimension (:), allocatable :: orig_wetvol,orig_wetsed
      real, dimension (:), allocatable :: orig_wetno3,orig_wetsolp
      real, dimension (:), allocatable :: orig_wetorgn,orig_wetorgp
      real, dimension (:), allocatable :: orig_solcov,orig_solsw
      real, dimension (:), allocatable :: orig_potno3,orig_potsed
      real, dimension (:), allocatable :: wtab,wtab_mn,wtab_mx
      real, dimension (:), allocatable :: shallst_n,gw_nloss,rchrg_n
      real, dimension (:,:), allocatable :: rfqeo_30d,eo_30d
      real, dimension (:,:), allocatable :: wgncur,wgnold,wrt,psetlp
      real, dimension (:,:), allocatable :: zdb,pst_surq,pst_enr
      real, dimension (:,:), allocatable :: plt_pst,pst_sed,psetlw
      real, dimension (:,:), allocatable :: pcpband,wupnd,tavband,phi
      real, dimension (:,:), allocatable :: wushal,wudeep,tmnband,snoeb
      real, dimension (:,:), allocatable :: surf_bs,nsetlw,snotmpeb,bss
      real, dimension (:,:), allocatable :: tmxband,nsetlp
      real, dimension (:,:), allocatable :: rainsub,hhsubp,frad
      real, dimension (:,:), allocatable :: orig_snoeb,orig_pltpst
      real, dimension (:,:,:), allocatable :: pst_lag
      integer, dimension (:), allocatable :: ipot,nrelease,swtrg,hrupest
      integer, dimension (:), allocatable :: nro,nrot,nfert,npest
      integer, dimension (:), allocatable :: igro,nair,ipnd1,ipnd2
      integer, dimension (:), allocatable :: nirr,iflod1,iflod2,ndtarg
      integer, dimension (:), allocatable :: nmgt,icr,ncut,nsweep,nafert
      integer, dimension (:), allocatable :: irn,irrno,sol_nly,npcp
      integer, dimension (:), allocatable :: igrz,ndeat,ngr,ncf
      integer, dimension (:), allocatable :: idorm,urblu,hru_sub,ldrain
      integer, dimension (:), allocatable :: hru_seq
      integer, dimension (:), allocatable :: iurban,iday_fert,icfrt
      integer, dimension (:), allocatable :: ndcfrt,irip,ifld,hrugis
      integer, dimension (:), allocatable :: orig_igro,ntil,irrsc
      integer, dimension (:), allocatable :: iwatable,curyr_mat
      integer, dimension (:), allocatable :: ncpest,icpst,ndcpst
      integer, dimension (:), allocatable :: iday_pest
      integer, dimension (:,:), allocatable :: rndseed
      real, dimension (:), allocatable :: wshd_pstap, wshd_pstdg
      integer, dimension (:), allocatable :: ndmo,npno
      character(len=13), dimension (18) :: rfile,tfile
      character(len=1), dimension (6300) :: hydgrp, kirr
      character(len=4), dimension (50) :: urbname
      character(len=16), dimension (6300) :: snam
      character(len=17), dimension (300) :: pname
      character(len=13) :: heds(68),hedb(18),hedr(42),hedrsv(41)
      character(len=13) :: hedwtr(40)
      character(len=4) :: title(60), cpnm(250)
! measured input files
      real, dimension (:,:,:), allocatable :: flomon,solpstmon,srbpstmon
      real, dimension (:,:,:), allocatable :: sedmon,orgnmon,orgpmon
      real, dimension (:,:,:), allocatable :: no3mon,minpmon,nh3mon
      real, dimension (:,:,:), allocatable :: no2mon,bactpmon,bactlpmon
      real, dimension (:,:,:), allocatable :: cmtl1mon,cmtl2mon,cmtl3mon
      real, dimension (:,:,:), allocatable :: chlamon,disoxmon,cbodmon
      real, dimension (:,:), allocatable :: floyr,sedyr,orgnyr,orgpyr
      real, dimension (:,:), allocatable :: no3yr,minpyr,nh3yr,no2yr
      real, dimension (:,:), allocatable :: bactpyr,bactlpyr,cmtl1yr
      real, dimension (:,:), allocatable :: cmtl2yr,cmtl3yr,chlayr
      real, dimension (:,:), allocatable :: disoxyr,cbodyr,solpstyr
      real, dimension (:,:), allocatable :: srbpstyr
      real, dimension (:), allocatable :: flocnst,sedcnst,orgncnst
      real, dimension (:), allocatable :: orgpcnst,no3cnst,minpcnst
      real, dimension (:), allocatable :: nh3cnst,no2cnst,bactpcnst
      real, dimension (:), allocatable :: cmtl1cnst,cmtl2cnst,bactlpcnst
      real, dimension (:), allocatable :: cmtl3cnst,chlacnst,disoxcnst
      real, dimension (:), allocatable :: cbodcnst,solpstcnst,srbpstcnst

! hourly time step (by AVG)
      integer :: idt, nstep
      real, dimension (:), allocatable :: hrtwtr,hhstor,hdepth,hsdti
      real, dimension (:), allocatable :: hrchwtr,halgae,horgn,hnh4
      real, dimension (:), allocatable :: hno2,hno3,horgp,hsolp,hbod
      real, dimension (:), allocatable :: hdisox,hchla,hsedyld,hsedst
      real, dimension (:), allocatable :: hharea,hsolpst,hsorpst
      real, dimension (:), allocatable :: hhqday,hhprecip,precipdt
      real, dimension (:), allocatable :: hhtime,hbactp,hbactlp
! store initial values
      integer, dimension (:), allocatable :: ivar_orig
      real, dimension (:), allocatable :: rvar_orig
! Input Uncertainty, added by Ann van Griensven
      integer ::  iseed, nauto, imocheck,idlast
	double precision ::  psamp
	integer, dimension (:), allocatable :: isamp, itelmon
      integer, dimension (:,:), allocatable :: ipinue
      real, dimension (:,:), allocatable :: stprain, stptemp, stprad
	real, dimension (:,:), allocatable :: stprhd, stppst, stpfrt
        real, dimension (:,:), allocatable :: variimon
	real, dimension (:,:,:), allocatable :: stpcst
! additional reach variables , added by Ann van Griensven
        real, dimension (:), allocatable :: wattemp
! Modifications to Pesticide and Water routing routines by Balaji Narasimhan
        real, dimension (:), allocatable :: lkpst_mass, lkspst_mass
        real, dimension (:), allocatable :: vel_chan

      end module parm

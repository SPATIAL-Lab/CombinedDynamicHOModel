
############################
####### HO FUNCTIONS #######
############################


##### HO driver function ##### 

HOdriver = function (Angry_Bird) { 
  # Table for a single B1 bird from AB model (i.e., Jim) 
	

  ### 1- Times at which data should be saved in the output matrix ###
  
  Tot.hours = nrow(Angry_Bird)
  
  tstep = 1 # Initiate the model at timestep 1
  times = seq(0, Tot.hours-1, by = tstep) 
  
  
  ### 2- Initial values ###	
  
  ## Food model ## 
  				
  # Amount of energy stored [kJ]
  TBE = M * p_reserves * energy_per_reserve_unit  
  
  # Amount of energy stored [mol O2], corresponding to [mol H2] and [mol 16O2]
  O2 = TBE * CF
  
  # HO isotope compositions of res pool
  # Assumes initial values equal to SS values
  # Assumes a single undifferentiated res pool but tracks two O isotope 
  #compositions: 1-that of carb+fat component of res pool, which is 
  #influenced by O isotope compositions of ingested food (f(d18Opcarb,
  #d18Opfat)); and 2-that of prot component, which is additionally influenced 
  #by the O exchange of amino acids with gut water during digestion 
  #( f(d18Opprot,d18Ogw))
  d2Hres = round(ssBird$d2Hres, digits=2)
  d18Ores = round(ssBird$d18Ores, digits=2)
  d18Oresprot = round(ssBird$d18Oresprot, digits=2)  
  RHres = delta.to.R(d2Hres, RstandardH)
  ROres = delta.to.R(d18Ores, RstandardO)
  ROresprot = delta.to.R(d18Oresprot, RstandardO)
  
  # Amount of deuterium in carb+fat+prot component of res pool ('res') [mol D2]
  D2res = RHres * O2 
  
  # Amount of oxygen-18 in in carb+fat component of res pool ('res') [mol 18O2] 
  O_18res = ROres * O2 * (Pcarb+Pfat)  
  
  # Amount of oxygen-18 in in prot component of res pool ('resprot') [mol 18O2] 
  O_18resprot = ROresprot * O2 * Pprot 
  
  ## HO model ##
  
  # Amount of total body water [g]
  TBW = M * pTBW    
  				
  # Amount of total body water [mol H2O], corresponding to [mol H2] and [mol 16O]
  H2 = TBW / MwH2O 
  
  # HO isotope compositions of body water
  # Assumes initial values equal to SS values
  d2Hbw = round(ssBird$d2Hbw, digits=2)
  d18Obw = round(ssBird$d18Obw, digits=2)
  RHbw = delta.to.R(d2Hbw, RstandardH)
  RObw = delta.to.R(d18Obw, RstandardO)
  
  # Amount of deuterium in body water [mol D2]
  D2bw = RHbw * H2
  
  # Amount of oxygen-18 in body water [mol 18O]
  O_18bw = RObw * H2
  
  
  state = c("O2"=O2, "D2res"=D2res, "O_18res"=O_18res, "O_18resprot"=O_18resprot, 
            "H2"=H2, "D2bw"=D2bw, "O_18bw"=O_18bw)
  
  
  ### 3- Parameters ###
  
  
  ## Parameter values at each timestep
  BSs = Angry_Bird$BS[1:Tot.hours]
  Eat.nos = Angry_Bird$Eat.no[1:Tot.hours]
  DrinkYNs = Angry_Bird$DrinkYN[1:Tot.hours]
  Habitats = Angry_Bird$Habitat[1:Tot.hours] 
  Moves = Angry_Bird$MoveYN[1:Tot.hours]
  
  parameters = list("BSs"=BSs, "Eat.nos"=Eat.nos, "DrinkYNs"=DrinkYNs, 
                    "Habitats"=Habitats, "Moves" = Moves,
                    "HungerIncreaseNight"=HungerIncreaseNight, 
                    "HungerIncreaseDay"=HungerIncreaseDay)
  
  out = deSolve::ode(y = state, times = times, func= HOmodel, 
                     parms = parameters, method = "rk4")

return(out)

}


##### HO model function ##### 

HOmodel = function(t, state, parameters) {
  with(as.list(c(state, parameters)), {
  	
    # calcs from 1&2 already done in HO_parameters.R but also needed here 
    #BMR_O2 = BMR * (60*60*24/1000) * CF
    #FMR = (FMR_BMR * BMR_O2)
    #FMR = FMR * 2 #multiplied by 2 to rescale to data for songbirds
    #FMR = FMR / 24 #Scaled per hour 
    
    RMR = FMR /5 #(hunger growth = 1 at night) 
    
    # Parameter values at current timestep
    BS = BSs[t+1]
    Eat.no = Eat.nos[t+1]
    DrinkYN = DrinkYNs[t+1]
    Habitat = Habitats[t+1]
    Move = Moves[t+1]
    
    if(BS == "N"){
      Metab = HungerIncreaseNight
    } else{
      if(Move == 1){
        Metab = HungerIncreaseDay * 1.2
      } else{
        Metab = HungerIncreaseDay * 0.8
      }
    }
    
    MR_hour = Metab * RMR


    ## Food model ##
    
    # Flux of energy in per hour [(mol O2 Prey^-1)*(Prey h^-1)] = [mol O2 h^-1] 
    # Depends on how much you are eating at current timestep
    O2in_hour = RMR * Eat.no
    
    # Flux of energy out per hour [mol O2 hour^-1]
    # Assumed equal to FMR
    O2out_hour = MR_hour
    
    
    # HO isotope compositions of prey (diet) [per mil]
    
    if (substr(ex, 1, 1) == "E") { 
    ## Depend on habitat based on isoscape models
      
    # If Eat.no = 0, single d2H,d18O prey value is pulled but not used      	
    d18Od2Hp = if (round(Eat.no,digits=0) == 0) {   
    	if (Habitat == 1) {
    		mvrnorm(n = 1, mu = c(mean(d18Op.MEAD)-avgd18Ooff_MR,
    		                      mean(d2Hp.MEAD)-avgd2Hoff_MR), 
    		        Sigma = cov(cbind(d18Op.MEAD,d2Hp.MEAD)))
    	} else {
    		if (Habitat ==2) {
    			mvrnorm(n = 1, mu = c(mean(d18Op.MEAD),mean(d2Hp.MEAD)), 
    			        Sigma = cov(cbind(d18Op.MEAD,d2Hp.MEAD)))
    		} else {
    			mvrnorm(n = 1, mu = c(mean(d18Op.MEAD)+avgd18Ooff_SM,
    			                      mean(d2Hp.MEAD)+avgd2Hoff_SM), 
    			        Sigma = cov(cbind(d18Op.MEAD,d2Hp.MEAD)))
    		}
    	}
    } else { # If Eat.no is > 0, n d2H,d18O prey values are pulled - where 
      #n = Eat.no - and avg is used 
    	      	if (Habitat == 1) {
    		mvrnorm(n = round(Eat.no,digits=0), mu = c(mean(d18Op.MEAD)-
    		                                             avgd18Ooff_MR,
    		                                           mean(d2Hp.MEAD)-
    		                                             avgd2Hoff_MR), 
    		        Sigma = cov(cbind(d18Op.MEAD,d2Hp.MEAD)))
    	} else {
    		if (Habitat ==2) {
    			mvrnorm(n = round(Eat.no,digits=0), mu = c(mean(d18Op.MEAD),
    			                                           mean(d2Hp.MEAD)), 
    			        Sigma = cov(cbind(d18Op.MEAD,d2Hp.MEAD)))
    		} else {
    			mvrnorm(n = round(Eat.no,digits=0), mu = c(mean(d18Op.MEAD)+
    			                                             avgd18Ooff_SM,
    			                                           mean(d2Hp.MEAD)+
    			                                             avgd2Hoff_SM), 
    			        Sigma = cov(cbind(d18Op.MEAD,d2Hp.MEAD)))
    		}
    	}
    }
    
    names(d18Od2Hp) = NULL
    
    d18Od2Hp = matrix(d18Od2Hp, ncol=2)
    
    # prey d2H,d18O value for each prey eaten
    d18Op_each_p = d18Od2Hp[,1]
    d2Hp_each_p = d18Od2Hp[,2] 

    # avg prey d2H,d18O value for all prey eaten 
    d18Op = mean(d18Op_each_p)
    d2Hp = mean(d2Hp_each_p) 
    	
    } else {

    		d2Hp = ssBird$d2Hins 
    		d18Op = ssBird$d18Oins

    }

    RHp = delta.to.R(d2Hp, RstandardH)
    ROp = delta.to.R(d18Op, RstandardO)
    
    
    # HO isotope compositions of prey (dietary) macronutrients [per mil]
    
    # H isotope composition of prey protein [per mil]
    d2Hpprot = (d2Hp - offHpcarb_pprot*PLEpcarb) / (PLEpcarb + PLEpprot)
    RHpprot = delta.to.R(d2Hpprot, RstandardH)
    
    # H isotope composition of prey carbohydrate [per mil]
    d2Hpcarb = d2Hpprot + offHpcarb_pprot
    RHpcarb = delta.to.R(d2Hpcarb, RstandardH)
    
    # H isotope composition of prey lipid [per mil]
    d2Hpfat = d2Hpprot - offHpprot_pfat
    RHpfat = delta.to.R(d2Hpfat, RstandardH)
    
    # O isotope composition of prey protein [per mil]
    d18Opprot = (d18Op - offOpcarb_pprot*PLEpcarb) / (PLEpcarb + PLEpprot)
    ROpprot = delta.to.R(d18Opprot, RstandardO)
    
    # O isotope composition of prey carbohydrate [per mil]
    d18Opcarb = d18Opprot + offOpcarb_pprot
    ROpcarb = delta.to.R(d18Opcarb, RstandardO)
    
    # O isotope composition of prey lipid [per mil]
    d18Opfat = d18Opprot - offOpprot_pfat
    ROpfat = delta.to.R(d18Opfat, RstandardO)
    
    
    # H isotope composition of carb+fat+prot component of res pool 
    RHres = D2res / O2
    
    # O isotope composition of carb+fat component of res pool  
    ROres = O_18res / ((Pcarb+Pfat) * O2)
    
    # O isotope composition of prot component of res pool
    ROresprot =  O_18resprot / (Pprot * O2)
    
     
    ## HO model ##
    
    # Flux of metabolically derived H in [mol H2 d^-1]
    FfH_hour = rH * MR_hour 
    
    # Flux of food water in [mol H2O d^-1]
    # Depends on O2in (O2_per_prey*Eat.no)   
    Ffw_hour = ( (Ms/MwH2O) * (Pw/(1-Pw)) * RMR * Eat.no )
                
    # Flux of drinking water in [mol H2O d^-1] 
    # Depends on whether you are drinking at current time step
    # Determined so that avg dyn Fdw value approx. matches SS Fdw value, 
    # assuming that birds represented here are close to balance; 
    # also rescaled so that modeled TWF is consistent with measured TWF data 
    # for birds       
    Fdw = if(DrinkYN == 1) {   
      (((round(ssBird$Fdw,digits = 2) * 8) * 1.410256) * 25)/100   
    } else {
      0
    }
             
    Fdw_hour = Fdw / 24
    
    # Total water flux (actual) [mol H2O d^-1]
    TWFact = FfH_hour + Ffw_hour + Fdw_hour 
    
    # Flux of vapor water out (evaporative water loss) [mol H2O d^-1]
    Fvw = aEWL * M^bEWL
    Fvw_hour = Fvw / 24
    
    # Vapor loss 20% higher in slope, 20% lower in riparian
    if(Habitat == 1){
      Fvw_hour = Fvw_hour * 0.8
    } else if(Habitat == 3){
      Fvw_hour = Fvw_hour * 1.2
    }
    
    # vapor loss 20% higher than mean during movement, 20% lower when 
    # not moving  
    if(Move == 1){
      Fvw_hour = Fvw_hour * 1.2
    } else{
      Fvw_hour = Fvw_hour * 0.8
    }
    
    # Flux of waste H out [mol H2 d^-1]
    FwasteH_hour = rHwaste * Pprot * MR_hour
    
    # Flux of liquid water out [mol H2O d^-1]
    # Depends on H2
    # First condition allows for increased liquid water excretion to restore 
    #body water pool to minimum threshold size with e-folding time of 0.08 
    #d (2 h)
    # Second condition imposes minimum Flw flux that depletes body water pool 
    #at a rate of 10% of threshold size per 7 d  
    Flw = max((H2 - (M * pTBW)/MwH2O) / 0.08, ((M * pTBW)/MwH2O * 0.1) / 7)
    
    Flw_hour = Flw / 24
    
    # Flux of metabolically derived O in [mol O d^-1]
    FfO_hour = rO * MR_hour 
          
    # Flux of inhaled O2 in [mol O d^-1]
    FO2_hour =  MR_hour*2
    
    # Flux of waste O out [mol O d^-1]
    FwasteO_hour = rOwaste * Pprot * MR_hour
    
    # Flux of exhaled CO2 [mol O d^-1]
    FCO2_hour = Rq * (MR_hour*2) 
     
    
    # HO isotope compositions of prey water [per mil]
    
    if (substr(ex, 1, 1) == "E") {
    ## Depend on habitat based on isoscape models 	 
    
    # Source water, i.e., mean of stream water values	
    d18O_ew = mean(d18Oew.RIP)  
    # Highest prey water sample among habitats 
    d18O_max.evap = max(c(d18Opw.RIP, d18Opw.MEAD, d18Opw.SLP))
    # Maximum possible enrichment
    d18O_evap.scale = d18O_max.evap - d18O_ew 
    # % enrichment; maximum possible enrichment * % enrichment = expressed 
    #enrichment; % enrichment: right-skewed in riparian (most samples have 
    #little enrichment, a few have lots); symmetrical in meadow, left-skewed 
    #in slope
    evaps = if (round(Eat.no,digits=0) == 0) { 
      if (Habitat == 1) {
        rbeta(1,2,4)
      } else {
        if (Habitat == 2) {
          rbeta(1,2,2)
        } else {
          rbeta(1,4,2)
        }
      } 
    } else { 
      if (Habitat == 1) {
        rbeta(round(Eat.no,digits=0),2,4)
      } else {
        if (Habitat == 2) {
          rbeta(round(Eat.no,digits=0),2,2)
        } else {
          rbeta(round(Eat.no,digits=0),4,2)
        }
      } 
    }
    # O isotope composition of prey water from each prey
    d18Opw_each_p = d18O_ew + d18O_evap.scale * evaps
    # O isotope composition of prey water 
    d18Opw = mean(d18Opw_each_p) 
    
    d2Hdata = c(d2Hpw.RIP,d2Hpw.MEAD,d2Hpw.SLP)
    d18Odata = c(d18Opw.RIP,d18Opw.MEAD,d18Opw.SLP)
    # Regress prey water d18O and d2H data 
    regMod = lm(d2Hdata ~ d18Odata)
    # Predict prey water d2H from d18O
    d2Hpw_pred = predict(regMod, data.frame(d18Odata = d18Opw))
    names(d2Hpw_pred) = NULL 
    # Add dispersion
    d2Hpw = d2Hpw_pred + rnorm(1, mean(regMod$residuals), sd(regMod$residuals))
    	
    } else {
    	
    		d2Hpw = ssBird$d2Hinsw 
    		d18Opw = ssBird$d18Oinsw

    }

    RHpw = delta.to.R(d2Hpw, RstandardH)
    ROpw = delta.to.R(d18Opw, RstandardO)
    
    
    # HO isotope compositions of environmental water [per mil]

    if (substr(ex, 1, 1) == "E") { 
    ## Depend on habitat based on isoscape models
    	
      if (Habitat == 1) {
      	
      	d18Od2Hew = mvrnorm(n = 1, mu = c(mean(d18Oew.RIP),mean(d2Hew.RIP)), 
      	                    Sigma = cov(cbind(d18Oew.RIP,d2Hew.RIP)))
      	
      } else {
        if (Habitat == 2) {
      		
        	d18Od2Hew.RIP = mvrnorm(n = 1, mu = c(mean(d18Oew.RIP),mean(d2Hew.RIP)), 
        	                        Sigma = cov(cbind(d18Oew.RIP,d2Hew.RIP)))
    
        	Ooff = rnorm(1, mean=2, sd=2/2)
        	Hoff = Ooff * runif(1, min=4, max=5) 
        
        	d18Oew.MEAD = d18Od2Hew.RIP[1] + Ooff 
        	d2Hew.MEAD = d18Od2Hew.RIP[2] + Hoff 
        
        	d18Od2Hew.MEAD = c(d18Oew.MEAD, d2Hew.MEAD)
        		
        	d18Od2Hew = d18Od2Hew.MEAD
      		
        } else {
        	
        	d18Od2Hew.RIP = mvrnorm(n = 1, mu = c(mean(d18Oew.RIP),mean(d2Hew.RIP)), 
        	                        Sigma = cov(cbind(d18Oew.RIP,d2Hew.RIP)))
    
        	Ooff = rnorm(1, mean=6, sd=6/2)
        	Hoff = Ooff * runif(1, min=4, max=5) 
        
        	d18Oew.SLP = d18Od2Hew.RIP[1] + Ooff 
        	d2Hew.SLP = d18Od2Hew.RIP[2] + Hoff 
        
        	d18Od2Hew.SLP = c(d18Oew.SLP, d2Hew.SLP)
        		
        	d18Od2Hew = d18Od2Hew.SLP
        	
        }
      } 
    names(d18Od2Hew) = NULL
    
    d18Oew = d18Od2Hew[1]
    d2Hew = d18Od2Hew[2]
    	
    } else {
    	
  		d2Hew = ssBird$d2Hew   
  		d18Oew = ssBird$d18Oew

    }
    
    RHew = delta.to.R(d2Hew, RstandardH)
    ROew = delta.to.R(d18Oew, RstandardO)
    
    
    # H isotope composition of metabolic H
    RHf = RHres
    d2Hf = R.to.delta(RHf, RstandardH)
    
    # H isotope composition of food water
    RHfw = RHpw
    d2Hfw = R.to.delta(RHfw, RstandardH)

    # H isotope composition of drinking water
    RHdw = RHew 
    d2Hdw = R.to.delta(RHdw, RstandardH)
    
    # O isotope composition of metabolic O; assumes that all macronutrients in 
    #the res pool are metabolized
    ROf = ROres*(Pcarb+Pfat) + ROresprot*Pprot
    d18Of = R.to.delta(ROf, RstandardO)
    
    # O isotope composition of food water
    ROfw = ROpw
    d18Ofw = R.to.delta(ROfw, RstandardO)
    
    # O isotope composition of drinking water
    ROdw = ROew
    d18Odw = R.to.delta(ROdw, RstandardO)
     
    # H isotope composition of body water
    RHbw = D2bw / H2
    	  
    # O isotope composition of body water
    RObw = O_18bw / H2
    
    # O isotope composition of gut water; food water d18O influences keratin 
    #d18O during digestion(in the gut); thus, ultimately both 1- ingested 
    #protein d18O, and 2-gut water d18O influence keratin d18O through res 
    #pool d18O 
    ROgw = g1 * RObw + g2 * ROdw + (1 - g1 - g2) * ROfw
    d18Ogw = R.to.delta(ROgw,RstandardO)

    
    ### MASTER EQNS ###

    dO2 = O2in_hour - O2out_hour
    dD2res = O2in_hour*RHp - O2out_hour*RHres
    dO_18res = (O2in_hour*(Pcarb*ROpcarb + Pfat*ROpfat)) - 
      (O2out_hour*ROres*(Pcarb + Pfat)) 
    dO_18resprot = (O2in_hour*(PfO*ROpprot + (1-PfO)*alphaOc_w*ROgw)*Pprot) - 
      (O2out_hour*ROresprot*Pprot) 
    
    dH2 = FfH_hour + Ffw_hour + Fdw_hour - (Fvw_hour + FwasteH_hour + Flw_hour)
    dD2bw = FfH_hour*RHf + Ffw_hour*RHfw + Fdw_hour*RHdw - 
      (Fvw_hour*RHbw*alphaHbw_vw + FwasteH_hour*RHbw + Flw_hour*RHbw)	
    dO_18bw = FfO_hour*ROf + Ffw_hour*ROfw + Fdw_hour*ROdw + 
      FO2_hour*ROo2*alphaOatm_abs - 
      (Fvw_hour*RObw*alphaObw_vw + FwasteO_hour*RObw + Flw_hour*RObw + 
         FCO2_hour*RObw*alphaObw_CO2)

		return(list(
    		
  		c(dO2, dD2res, dO_18res, dO_18resprot, dH2, dD2bw, dO_18bw),
  		
  		c("BS"=BS, "Eat.no"=Eat.no, "Habitat"=Habitat, 
    		"O2in"=O2in_hour, "O2out"=O2out_hour, 
    		"d2Hins"=d2Hp, "d2Hinsprot"=d2Hpprot, "d2Hinscarb"=d2Hpcarb, 
    		"d2Hinsfat"=d2Hpfat, "d18Oins"=d18Op, "d18Oinscarb"=d18Opcarb, 
    		"d18Oinsfat"=d18Opfat, "d18Oinsprot"=d18Opprot, "d18Ogw"=d18Ogw,
    		"d2Hres"=R.to.delta(D2res/O2,RstandardH), 
    		"d18Ores"=R.to.delta(O_18res/(Pcarb+Pfat)/O2,RstandardO), 
    		"d18Oresprot"=R.to.delta(O_18resprot/Pprot/O2,RstandardO)
  		  ), 
  		c("DrinkYN"=DrinkYN, "FMR"=MR_hour, "FfH"=FfH_hour, "Ffw"=Ffw_hour, 
  		  "Fdw"=Fdw_hour, "Fvw"=Fvw_hour, "FwasteH"=FwasteH_hour, "Flw"=Flw_hour, 
  		  "FfO"=FfO_hour, "FO2"=FO2_hour, "FwasteO"=FwasteO_hour, "FCO2"=FCO2_hour, 
    		"d2Hew"=d2Hew, "d2Hinsw"=d2Hpw, 
    		"d2Hdw"=d2Hdw, "d2Hfw"=d2Hfw, "d2Hf"=d2Hf, 
    		"d18Oew"=d18Oew, "d18Oinsw"=d18Opw, 
    		"d18Odw"=d18Odw, "d18Ofw"=d18Ofw, "d18Of"=d18Of, "d18Oo2"=d18Oo2, 
    		"d2Hbw"=R.to.delta(D2bw/H2,RstandardH), 
  		  "d18Obw"=R.to.delta(O_18bw/H2,RstandardO)
  		  )
    		
		)) 
  }	
  )	
}



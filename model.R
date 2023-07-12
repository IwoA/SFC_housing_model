#devtools::install_github("gamrot/godley")

library(godley)

model_sim <- create_model(name = "Housing")
model_sim <- model_sim |>
  add_variable(
     'Y', desc = 'National income',
     'YDcr', desc='Regular Disposable Income - capitalist',
     'YDwr', desc='Regular Disposable Income - workers',
     'T', desc='Taxes',
     'V', desc='Wealth',
     'Vc', init = 40, desc='Wealth - capitalists',
     'Vw', init = 40, desc='Wealth - workers',
     'CGc' , desc='Capital Gains - capitalists',
     'C' , desc='Consumption',
     'Cc' , desc='Consumption - capitalists',
     'Cw' , desc='Consumption - workers',
     'Vce' , desc='Expected wealth - capitalists',
     'Dch' , desc='Deposits held by capitalsts',
     'Dwh' , desc='Deposits held by workers',
     'BLcd' , desc='Demand for gov bonds',
     'Hcd' , init = 0.1, desc='Total capitalist demand for houses',
     'Mh' , desc='Mortgages held by banks',
     'PB', desc='Profits of banks',
     'D', desc="Deposits",
     'BLch' , desc='Bonds held by capitalists',
     'BLbh' , desc='Bonds held by banks',
     'Ms' , desc='Supply of mortgages',
     'Md' , desc='Demand for mortgages',
     'Ds' , desc='Deposits supplied by commercial banks',
     'BLs' , desc='Supply of gov bonds',
     'ErBL' , desc='Expected rate of return on bonds',
     'rBL' , desc='Interest rate on bonds',
     'YDer' , desc='Expected regular disposable income',
     'YDcer' , desc='Expected regular disposable income - capitalists',
     'YDwer' , desc='Expected regular disposable income - workers',
     'NHd' , desc='Demand for new houses',
     'NHcd' , desc='Demand for new houses - capitalists',
     'NHwd' , desc='Demand for new houses - workers', # równy jest supply of mortgages/phc
     'Hs' , init = 6, desc='Supply of houses',
     'NHch' , desc='New houses held by capitalists',
     'NHwh' , desc='New houses held by workers',
     'NHgh' , desc='New houses held by gov',
     'ph' , init = 1, desc='Price of house',
     'phg' , init = 0.6, desc='Price of house paid by government',
     'phc' , init = 1, desc='Price of house paid by non-government',
     'CGd' , desc='Capital gains on houses',
     'Hc' , init = 0.5, desc='Total number of houses held by capitalists',
     'Hw' , desc='Total number of houses held by workers',
     'Hg' , desc='Total number of houses held by gov',
     'Hh' , desc='New houses held',
     'Hu' , desc='Unsold houses',
     'ERrd' , desc='Expected rate of return on houses',
     'rh' , desc='Interest rate on houses',
     'rentc', desc='Rents paid by workers to capitalists',
     'rentg', desc='Rents paid by workers to government',
     'W', desc="Wages in housing sector",
     'P', desc="Capitalists profits from production of houses",
     # parameters
      'G', init = 60,  desc='Public Expenditures',
      'Trate', init = 0.05,  desc='Tax rate',
      'alpha1', init = 0.8,  desc='Marginal propensity to consume (income,',
      'alpha2', init = 0.2,  desc='Marginal propensity to consume (wealth,',
      'lam20', init = 0.44196,  desc='Portfolio parameter',
      'lam21', init = 0,  desc='Portfolio parameter',
      'lam22', init = 1.1,  desc='Portfolio parameter',
      'lam23', init = 1,  desc='Portfolio parameter',
      'lam24', init = 0.03,  desc='Portfolio parameter',
      'lam30', init = 0.3997,  desc='Portfolio parameter',
      'lam31', init = 0,  desc='Portfolio parameter',
      'lam32', init = 1,  desc='Portfolio parameter',
      'lam33', init = 1.1,  desc='Portfolio parameter',
      'lam34', init = 0.03,  desc='Portfolio parameter',
      'chi', init = 0.1,  desc='Expectation about bonds price',
      'rd', init = 0,  desc='Rate of interest of deposits',
      'rb', init = 0.03,  desc='Rate of interest of mortgages',
      'pBL', init = 50,  desc='Price of bonds',
      'Z', init = 0.1,  desc='Expectation about house prices',
      'NHgd', init = 0,  desc='Gov demand for new houses',
      'rg', init = 0.02,  desc='Rents of public houses',
      'phparam', init = 0.0005,  desc="House price elasticity to unsold houses",
      'dsparam', init = 0.02,  desc="House supply elasticity to price",
      'phgparam', init = 1.01,  desc="Price of houses paid by government relative to commercial price",
      'Wparam', init = 0.9,  desc="Share of costs of houses production paid by government",
      'hsparam', init = 1.5,  desc="Price elasticity of supply of houses",
      'mparam', init = 0.1,  desc="Share of workers disposable income which could be spent on mortgages"
  )

model_sim$variables

model_sim <- model_sim |>
  add_equation(
     'Y=C+G',
     'YDcr=0.5*(Y-T)+PB+rBL*BLch[-1]+rentc+P+rd*Dch',
     'rentc = Hc[-1]*phc[-1]*rh[-1]',
     'P=Hh*ph-W',
    
     'W=Wparam*phg*Hs',
     'rentg = Hg[-1]*rg[-1]*phg[-1]',
     'YDwr=0.5*(Y-T) - rb*Mh[-1] - (rh*Mh[-1] + sqrt(Mh[-1])) - rentg - rentc + W + rd*Dwh', 
     'T=Trate*Y', 
     'V=Vc+Vw',
     'Vc=Vc[-1]+(YDcr-Cc) + CGc',
     'Vw=Vw[-1]+(YDwr-Cw) + Hw[-1]*d(ph)-Mh',
     'CGc=d(pBL)*BLch[-1]+d(ph)*Hc[-1]',
     'C=Cc+Cw',
     'Cc=(alpha1*YDcer)+(alpha2*Vc[-1])',#
     'Cw=(alpha1*YDwer)+(alpha2*Vw[-1])',
     'Vce=Vc[-1]+(YDcer-Cc)',
     'Dch=Vc-BLcd*pBL-Hc*ph',  
     'Dwh=Vw-Hw*ph',  
     'D=Dwh+Dch',
     'BLcd=(Vce*lam20+Vce*ErBL*lam22-Vce*ERrd*lam23-lam24*YDcer)/pBL',  #wersja PL z pysolve3
     'Hcd= (Vce*lam30-Vce*ErBL*lam32+Vce*ERrd*lam33-lam34*YDcer)/phc',  #wersja PL z pysolve3
    
     'Mh=max(Mh[-1]-(rh*Mh[-1]+sqrt(Mh[-1]))+NHwh*ph,0)',
    
     'BLch=max(BLcd,0)',
     'BLbh=BLs-BLch',
     'PB=rb[-1]*Mh[-1]+rBL*BLbh[-1]-rd[-1]*D[-1]',
     'Ms=mparam*YDwr[-1]',  
     'Ds=Dch+Dwh',
     'BLs=G-rentg-T+rBL*BLch[-1]',
     'ErBL=rBL[-1]',
     'rBL=1/pBL',
     'YDer=YDcr[-1]+YDwr[-1]',
     'YDcer=YDcr[-1]',
     'YDwer=YDwr[-1]',
    
     'Hs= Hs[-1]*(1+ hsparam*d(ph)/ph[-1])',
     # Demand for houses
     'NHd = NHwd+NHcd+NHgd',
     'NHcd=Hcd-Hc[-1]',
     'NHwd=Ms/phc',
    
     'NHch=min((Hs-NHgh)*(NHcd/(NHcd+NHwd)),NHcd)', 
     'NHwh=min((Hs-NHgh)*(NHwd/(NHcd+NHwd)),NHwd)', 
     'Md=NHwh*phc',
     'NHgh = NHgd', 
     'phc=(phparam*(NHcd+NHwd)/(Hs-NHgd)*phc[-1]+phc[-1])',  
     'ph=((NHch+NHwh)/(Hs-Hu))*phc+(NHgh/(Hs-Hu))*phg', 
     'phg=phgparam*phg[-1]',
     'Hc = NHch+ Hc[-1]',
     'Hw =NHwh + Hw[-1]',
     'Hg = NHgh + Hg[-1]',
     'CGd=d(ph)*Hc[-1]',  
     'Hh=NHgh+NHch+NHwh',
     'Hu=Hs-NHgh-NHch-NHwh',
     'ERrd=(rentc[-1]+CGd[-1])/(ph[-1]*Hc[-1])', 
     'rh = rb'
    #"H_s = H_h", desc = "Money equilibrium", hidden = TRUE
  )

model_sim$equations

model_sim <- model_sim |>
  simulate_scenario(
    periods = 100, start_date = "2015-01-01",
    method = "Gauss", max_iter = 350, tol = 1e-05
  )

plot_simulation(
  model_sim, scenario = "baseline",
  from = "2015-01-01", to = "2023-01-01",
  expressions = c("Vc", "Vw")
)

model_sens <- model_sim |>
  create_sensitivity(
    variable = "alpha1", lower = 0.1, upper = 0.8, step = 0.1
  ) |>
  simulate_scenario(periods = 100, start_date = "2015-01-01")

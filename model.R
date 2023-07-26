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
     'phg' , init = 0.8, desc='Price of house paid by government',
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
      #'chi', init = 0.1,  desc='Expectation about bonds price',
      'rd', init = 0,  desc='Rate of interest of deposits',
      'rb', init = 0.03,  desc='Rate of interest of mortgages',
      'pBL', init = 50,  desc='Price of bonds',
      #'Z', init = 0.1,  desc='Expectation about house prices',
      'NHgd', init = 0,  desc='Gov demand for new houses',
      'rg', init = 0.02,  desc='Rents of public houses',
      'phparam', init = 0.0005,  desc="House price elasticity to unsold houses",
      'phgparam', init = 0.8,  desc="Price of houses paid by government relative to commercial price",
      'Wparam', init = 0.9,  desc="Share of costs of houses production (wages) in prices paid by government",
      'hsparam', init = 1.5,  desc="Price elasticity of supply of houses",
      'mparam', init = 0.1,  desc="Share of workers disposable income which could be spent on mortgages"
  )

#model_sim$variables

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
     'phg=phgparam*phc[-1]',
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

#model_sim$equations

model_sim_base <- model_sim |>
  simulate_scenario(
    periods = 50, start_date = "2015-01-01",
    method = "Gauss", max_iter = 350, tol = 1e-05
  )

plot_simulation(
  model_sim_base, scenario = "baseline",
  from = "2015-01-01", to = "2023-01-01",
  expressions = c("Vc", "Vw")
)


model_sim_housing <- model_sim |>  
  change_init('G', 61.5) |> 
  change_init('NHgd', 2.5 )
  # change_init('alpha1', 0.8) |>
  # change_init('alpha2', 0.2) |>
  # change_init('chi', 0.1) |>
  # change_init('lam20', 0.44196) |>
  # change_init('lam22', 1.1)|>
  # change_init('lam23', 1 )|>
  # change_init('lam24', 0.03 )|>
  # change_init('lam30', 0.3997 )|>
  #  change_init('lam32', 1 )|>
  #  change_init('lam33', 1.1 )|>
  #  change_init('lam34', 0.03 )|>
   # change_init('phparam', 0.0005 )|> 
   # change_init('phgparam', 1.01 )|>
   # change_init('Wparam', 0.9 )|>
   # change_init('hsparam', 1.5 )|>
   # change_init('mparam', 0.1 )|>
   # change_init('Z', 0.1 )|>
   # change_init('Trate', 0.05 )|>
   # change_init('pBL', 50)|> #20
   # change_init('rb', 0.03)|>
   # change_init('rg', 0.02) |> 
   # change_init('Vc', 40)|>
   # change_init('Vw', 40)|>
   # change_init('Hs', 6)|>
   # change_init('Hc', 0.5)|>
   # change_init('Hcd', 0.1)|>
   # change_init('phc', 1)|>
   # change_init('phg', 0.6)|>
   # change_init('ph', 1)

model_sim_housing <- model_sim_housing |>
  simulate_scenario(
    periods = 100, start_date = "2015-01-01",
    method = "Gauss", max_iter = 350, tol = 1e-05
  )

plot_simulation(
  model_sim_housing, scenario = "baseline",
  from = "2015-01-01", to = "2023-01-01",
  expressions = c("Vc", "Vw")
)

#dodanie do modelu bazowego wynikow z wersji housing
model_sim_base$housing <- model_sim_housing$baseline

# wykres baseline i housing
plot_simulation(
  model_sim_base, scenario = c("housing", "baseline"),
  from = "2015-01-01", to = "2023-01-01",
  expressions = c("Vc", "Vw")
)



model_sens <- model_sim_housing |>
  create_sensitivity(
    variable = "NHgd", lower = 0, upper = 4.5, step = 0.5) |>
  simulate_scenario(periods = 100, start_date = "2015-01-01")

# ZASKAKUJACY WYNIK DLA NHwd = 4.5
plot_simulation(model = model_sens, scenario = "sensitivity", take_all = TRUE,
                from = "2015-01-01", to = "2023-01-01", expressions = c("ph"))

plot_simulation(model = model_sens, scenario = "sensitivity", take_all = TRUE,
                from = "2015-01-01", to = "2023-01-01", expressions = c("phc"))

plot_simulation(model = model_sens, scenario = "sensitivity", take_all = TRUE,
                from = "2015-01-01", to = "2023-01-01", expressions = c("NHd"))

plot_simulation(model = model_sens, scenario = "sensitivity", take_all = TRUE,
                from = "2015-01-01", to = "2023-01-01", expressions = c("NHwd"))

model_sens <- model_sim_housing |>
  create_sensitivity(
    variable = "NHgd", lower = 0, upper = 5.5, step = 0.5) |>
  simulate_scenario(periods = 50, start_date = "2015-01-01")

# Sensivity wielu parametrów

# lista parametrów
params <- list(
  'G' =  data.frame('nazwa' = 'G', 'init'=60, 'od' = 50, 'do' = 120, 'step' = 10),
  'Trate' =  data.frame('nazwa' = 'Trate', 'init'=0.05, 'od' = 0.04, 'do' = 0.16, 'step' = 0.02), 
  'rd' =  data.frame('nazwa' = 'rd', 'init'=0, 'od' = 0, 'do' = 0.1, 'step' = 0.02),
  'rb' =  data.frame('nazwa' = 'rb', 'init'=0.03, 'od' = 0.03, 'do' = 0.12, 'step' = 0.03),
  'pBL' =  data.frame('nazwa' = 'pBL', 'init'=50, 'od' = 10, 'do' = 50, 'step' = 10),   #'rBL=1/pBL'
  'NHgd' =  data.frame('nazwa' = 'NHgd', 'init'=0, 'od' = 0, 'do' = 4.5, 'step' = 0.5),
  'rg' =  data.frame('nazwa' = 'rg', 'init'=0.02, 'od' = 0.01, 'do' = 0.1, 'step' = 0.02),   #rentg = Hg[-1]*rg[-1]*phg[-1]
  'phparam' =  data.frame('nazwa' = 'phparam', 'init'=0.0005, 'od' = 0.0002, 'do' = 0.001, 'step' = 0.0002),
  'phgparam' =  data.frame('nazwa' = 'phgparam', 'init'=0.8, 'od' = 0.5, 'do' = 0.9, 'step' = 0.1),
  'hsparam' =  data.frame('nazwa' = 'hsparam', 'init'=1.5, 'od' = 0.5, 'do' = 1.5, 'step' = 0.2),
  'mparam' =  data.frame('nazwa' = 'mparam', 'init'=0.1, 'od' = 0.01, 'do' = 0.2, 'step' = 0.05)
)


sensivity <- function(model, x, co){
  # funkcja tworzy listę wykresów
  # x = tabela z danymi do funkcji create_sensivity
  # c = zmienna, która ma być na wykresie
  
  model_sens <- 
    create_sensitivity(model,
                        variable = x$nazwa, lower = x$od, upper = x$do, step = x$step) |>
    simulate_scenario(periods = 50, start_date = "2015-01-01") 
  plot_simulation(model = model_sens, scenario = "sensitivity", take_all = TRUE,
                  from = "2015-01-01", to = "2023-01-01", expressions = c(co))
}

wynik <- purrr::map(params, \(x) sensivity(model_sim_housing ,x, co =c('Vc', 'Vw')))
plotly::subplot(wynik, nrows = 3) # rysuje wszystkie wyniki na jednym wykresie


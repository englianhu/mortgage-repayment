loanamort <- function(r = NULL, bal0 = NULL, pmt = NULL, n = NULL, 
                      apr = FALSE, start = NULL, freq = 1) {
  ## https://rpubs.com/kpolen/16816
  ## 
  ## The loan amort function
  ## 
  ## We're now ready to right code. Let's make a function loanamort that takes 
  ##   as input the four variables r, n, pmt and bal0. It requires that at 
  ##   least three of these values are provided and if only three are provided 
  ##   it calculates the missing value. It then calculates an amortization 
  ##   table for a loan with these parameters.
  ## 
  ## To make the function more practical, we add three additional optional 
  ##   variables that allow you to return zoo objects with actual dates. 
  ##   start is a starting date of bal0. freq is the frequency of payments, 
  ##   12 for monthly. If freq is not equal to 1, then r will be converted 
  ##   to a rate for the frequency. apr is a logical variable indicating if 
  ##   the given rate is stated as an “annual percentage rate”. If TRUE, then 
  ##   r is divided by freq before calculating. Otherwise r will be converted 
  ##   to a rate for freq taking a root of 1+r based on freq. If apr is TRUE 
  ##   then n is multiplied by freq before calculations are done. In other 
  ##   words, if you want to analyze a thirty year mortgage loan quoted in the 
  ##   conventional way, you set apr to TRUE, provide the annual percentage 
  ##   rate for r and provide 30 for n.
  ## 
  ## The function returns a list of values for the four loan parameters plus 
  ##   time series for the loan parameters.
  ## 
  ## Here is the code for the function.
  
  require(zoo)
  require(lubridate)
  
  ans = list()
  risnull = is.null(r)
  bal0isnull = is.null(bal0)
  pmtisnull = is.null(pmt)
  nisnull = is.null(n)
  if (1 < sum(c(risnull, bal0isnull, pmtisnull, nisnull))) 
    stop("loanamort error -- need to provide at least three parameters")
  n.f = n
  if (apr) 
    n.f = n * freq
  if (!risnull) {
    if (apr) {
      r.f = r/freq
    } else {
      r.f = -1 + (1 + r)^(1/freq)
    }
  } else {
    cf = c(-bal0, rep(pmt, n.f))
    if (0 <= sum(cf)) {
      rootrange = c(0, 1.01)
    } else {
      rootrange = c(1, 1000)
    }
    d = (uniroot(function(d) {
      sum(cf * d^(0:n.f))
    }, rootrange))$root
    r.f = (1/d) - 1
  }
  d = 1/(1 + r.f)
  f = 1 + r.f
  if (pmtisnull) 
    pmt = (bal0 * r.f)/(1 - d^n.f)
  perp = pmt/r.f
  if (bal0isnull) 
    bal0 = perp - perp * (d^n)
  if (pmt <= (r.f * bal0)) 
    stop(paste(pmt, r.f * bal0, "payment must be greater than interest"))
  if (nisnull) 
    n.f = ceiling(log((1 - (bal0 * r.f)/pmt))/log(d))
  i = 1:n.f
  bal = pmax(0, ((bal0 * f^i) - (((pmt * f^i) - pmt)/r.f)))
  balall = c(bal0, bal)
  int = balall[i] * r.f
  prin = -diff(balall)
  if (!is.null(start)) {
    bal = zooreg(bal, start = start + 1/freq, freq = freq)
    int = zooreg(int, start = start + 1/freq, freq = freq)
    prin = zooreg(prin, start = start + 1/freq, freq = freq)
  }
  if (apr) {
    ans$r = r.f * freq
    ans$n = n.f/freq
  } else {
    ans$r = -1 + ((1 + r.f)^freq)
    ans$n = n.f
  }
  ans$pmt = pmt
  ans$bal0 = bal0
  ans$freq = freq
  ans$start = start
  ans$apr = apr
  ans$bal = bal
  ans$prin = prin
  ans$int = int
  return(ans)
}
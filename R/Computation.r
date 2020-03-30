#' Computes the expected demand
#' 
#' Computes the expected demand for a given Word-of-Mouth campaign at a given price.
#' 
#' @param campaign Word-of-Mouth campaign as instance of class \code{WoMCampaign}.
#' @param price Price as number in [0; 1] where 0 is the minimal and 1 is the maximal price.
#' @author Michael Scholz \email{michael.scholz@@th-deg.de}
#' @author Thomas Woehner \email{Thomas.Woehner@@eah-jena.de}
#' @author Ralf Peters \email{ralf.peters@@wiwi.uni-halle.de}
#' @return Expected demand in number of persons.
#' @seealso \code{\link{computeRoundDemand}} \code{\link{computeProfit}} \code{\link{computeConsumerSurplus}} \code{\link{computeOptimalPrice}}
#' @examples 
#' 
#' network <- new("WoMNetwork", size = 1000, avgConnections = 5)
#' campaign <- new("WoMCampaign", network = network, seedingSize = 10, forwardProbability = 0.2)
#' demand <- computeDemand(campaign, price = 0.5)
#' print(demand)
#' 
#' @export
computeDemand = function(campaign, price) {
    wom = 1 - (campaign@network@avgConnections * campaign@forwardProbability) / (campaign@network@size - 1)
    h = campaign@network@size*(1 - price)
    d0 = (1 - price)*campaign@seedingSize
    t = h - d0
    demand = (h*log(wom) - W(t*wom^h*log(wom))) / log(wom)
    return(demand)
}


#' Computes the expected demand per round
#' 
#' Computes the expected demand for a given Word-of-Mouth campaign at a given price and a given round or a given round and all previous rounds
#' 
#' @param campaign Word-of-Mouth campaign as instance of class \code{WoMCampaign}.
#' @param price Price as number in [0; 1] where 0 is the minimal and 1 is the maximal price.
#' @param round Round at which or until which the demand per round will be computed.
#' @param previousRounds Should the demand of all previous rounds be returned or not. Default is TRUE.
#' @author Michael Scholz \email{michael.scholz@@th-deg.de}
#' @author Thomas Woehner \email{Thomas.Woehner@@eah-jena.de}
#' @author Ralf Peters \email{ralf.peters@@wiwi.uni-halle.de}
#' @return Expected demand in number of persons. Note that the first value in the demand vector is the number of initial consumers 
#' when previousRounds is TRUE. The number of initial consumers is (1-p)*seedingSize.
#' @seealso \code{\link{computeDemand}} \code{\link{computeProfit}} \code{\link{computeConsumerSurplus}} \code{\link{computeOptimalPrice}}
#' @examples 
#' 
#' network <- new("WoMNetwork", size = 1000, avgConnections = 5)
#' campaign <- new("WoMCampaign", network = network, seedingSize = 10, forwardProbability = 0.2)
#' demand <- computeRoundDemand(campaign, price = 0.5, round = 3)
#' print(demand)
#' 
#' @export
computeRoundDemand = function(campaign, price, round, previousRounds = TRUE) {
    wom = 1 - (campaign@network@avgConnections * campaign@forwardProbability) / (campaign@network@size - 1)
    h = campaign@network@size*(1 - price)
    d0 = (1 - price)*campaign@seedingSize
    dprev = d0
    d = d0
    dt = d0
    for (t in 1:round) {
        dround = (1-wom^dprev)*(h-d)
        dt = c(dt, dround)
        dprev = dround
        d = d + dround
    }
    if (previousRounds) {
        return(dt)
    } else {
        return(dround)
    }
}

#' Computes the expected profit
#' 
#' Computes the expected profit for a given Word-of-Mouth campaign at a given price.
#' 
#' @param campaign Word-of-Mouth campaign as instance of class \code{WoMCampaign}.
#' @param price Price as number in [0; 1] where 0 is the minimal and 1 is the maximal price.
#' @author Michael Scholz \email{michael.scholz@@th-deg.de}
#' @author Thomas Woehner \email{Thomas.Woehner@@eah-jena.de}
#' @author Ralf Peters \email{ralf.peters@@wiwi.uni-halle.de}
#' @return Expected profit as number of persons times price.
#' @seealso \code{\link{computeDemand}} \code{\link{computeConsumerSurplus}} \code{\link{computeOptimalPrice}}
#' @examples 
#' 
#' network <- new("WoMNetwork", size = 1000, avgConnections = 5)
#' campaign <- new("WoMCampaign", network = network, seedingSize = 10, forwardProbability = 0.2)
#' profit <- computeProfit(campaign, price = 0.5)
#' print(profit)
#' 
#' @export
computeProfit = function(campaign, price) {
    demand = computeDemand(campaign, price)
    profit = demand * price
    return(profit)
}

#' Computes the expected cumulative consumer surplus
#' 
#' Computes the expected cumulative consumer surplus for a given Word-of-Mouth campaign at a given price.
#' 
#' @param campaign Word-of-Mouth campaign as instance of class \code{WoMCampaign}.
#' @param price Price as number in [0; 1] where 0 is the minimal and 1 is the maximal price.
#' @author Michael Scholz \email{michael.scholz@@th-deg.de}
#' @author Thomas Woehner \email{Thomas.Woehner@@eah-jena.de}
#' @author Ralf Peters \email{ralf.peters@@wiwi.uni-halle.de}
#' @return Expected cumulative consumer surplus.
#' @seealso \code{\link{computeDemand}} \code{\link{computeProfit}} \code{\link{computeOptimalPrice}}
#' @examples 
#' 
#' network <- new("WoMNetwork", size = 1000, avgConnections = 5)
#' campaign <- new("WoMCampaign", network = network, seedingSize = 10, forwardProbability = 0.2)
#' surplus <- computeConsumerSurplus(campaign, price = 0.5)
#' print(surplus)
#' 
#' @export
computeConsumerSurplus = function(campaign, price) {
    demand = computeDemand(campaign, price)
    surplus = demand*(1-price)/2
    return(surplus)
}

.gradPrice = function(price, campaign) {
    wom = 1 - (campaign@network@avgConnections * campaign@forwardProbability) / (campaign@network@size - 1)
    t1 = 1/log(wom)*(price)*(-(log(wom)*(campaign@seedingSize - campaign@network@size)*wom^(campaign@network@size*(1 - price)) - campaign@network@size*(log(wom))^2*wom^(campaign@network@size*(1 - price))*(campaign@network@size*(1 - price) - campaign@seedingSize*(1 - price))) * 
                                 deriv_W(log(wom)*wom^(campaign@network@size*(1 - price))*(campaign@network@size*(1 - price) - campaign@seedingSize*(1 - price))) - campaign@network@size*log(wom))
    t2 = (campaign@network@size*(1 - price)*log(wom) - W((campaign@network@size*(1 - price) - (1 - price)*campaign@seedingSize)*wom^(campaign@network@size*(1-price))*log(wom)))/log(wom)
    grad = t1 + t2
    return(grad)
}

#' Computes the profit-maximizing price
#' 
#' Computes the profit-maximizing for a given Word-of-Mouth campaign.
#' 
#' @param campaign Word-of-Mouth campaign as instance of class \code{WoMCampaign}.
#' @author Michael Scholz \email{michael.scholz@@th-deg.de}
#' @author Thomas Woehner \email{Thomas.Woehner@@eah-jena.de}
#' @author Ralf Peters \email{ralf.peters@@wiwi.uni-halle.de}
#' @return Profit-maximizing price in [0; 1] where 0 is the lowest possible and 1 is the highest possible price.
#' @seealso \code{\link{computeDemand}} \code{\link{computeProfit}} \code{\link{computeConsumerSurplus}}
#' @examples 
#' 
#' network <- new("WoMNetwork", size = 1000, avgConnections = 5)
#' campaign <- new("WoMCampaign", network = network, seedingSize = 10, forwardProbability = 0.2)
#' price <- computeOptimalPrice(campaign)
#' profit <- computeProfit(campaign, price)
#' print(price)
#' print(profit)
#' 
#' @export
computeOptimalPrice = function(campaign) {
    optPrice = optim(par = 0.5, fn = computeProfit, gr = .gradPrice, campaign = campaign,
                     lower = 0, upper = 1, method = "L-BFGS-B", control = list(fnscale = -1))$par
    return(optPrice)
}
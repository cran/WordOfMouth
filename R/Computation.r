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
        dround = (1 - wom^dprev)*(h - d)
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
    ci = ifelse(.hasSlot(campaign, "informationCosts") && length(campaign@informationCosts) > 0, 
                campaign@informationCosts, 0)
    profit = demand * price - campaign@seedingSize * ci
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
    surplus = demand*(1 - price)/2
    return(surplus)
}

.gradPrice = function(price, campaign) {
    wom = (campaign@network@avgConnections * campaign@forwardProbability) / (campaign@network@size - 1)
    fp = (campaign@network@size - campaign@seedingSize)*(1 - price)*(1 - wom)^(campaign@network@size*(1 - price))*log(1 - wom)
    fp1 = (campaign@network@size - campaign@seedingSize)*(1 - wom)^(campaign@network@size*(1 - price))*log(1 - wom)*(-1 + campaign@network@size*log(1 - wom)*(price - 1))
    grad = campaign@network@size*(1 - 2*price) - (deriv_W(fp) * fp1*price + W(fp)) / (log(1 - wom))
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


#' Computes the WoM intensity
#' 
#' Computes the WoM intensity in a given Word-of-Mouth campaign.
#' 
#' @param campaign Word-of-Mouth campaign as instance of class \code{WoMCampaign}.
#' @author Michael Scholz \email{michael.scholz@@th-deg.de}
#' @author Thomas Woehner \email{Thomas.Woehner@@eah-jena.de}
#' @author Ralf Peters \email{ralf.peters@@wiwi.uni-halle.de}
#' @return WoM intensity in [0; 1].
#' @examples 
#' 
#' network <- new("WoMNetwork", size = 1000, avgConnections = 5)
#' campaign <- new("WoMCampaign", network = network, seedingSize = 10, forwardProbability = 0.2)
#' intensity <- computeWoMIntensity(campaign)
#' print(intensity)
#' 
#' @export
computeWoMIntensity = function(campaign) {
    eta = campaign@network@avgConnections * campaign@forwardProbability / (campaign@network@size - 1)
    return(eta)
}


#' Computes the information costs threshold
#' 
#' Computes the information costs that need to be surpassed in order to generate a higher profit than in 
#' a transparent market.
#' 
#' @param campaign Word-of-Mouth campaign as instance of class \code{WoMCampaign}.
#' @author Michael Scholz \email{michael.scholz@@th-deg.de}
#' @author Thomas Woehner \email{Thomas.Woehner@@eah-jena.de}
#' @author Ralf Peters \email{ralf.peters@@wiwi.uni-halle.de}
#' @return Information costs in [0; 1] that need to be surpassed in order to generate a higher profit than in a 
#' transparent market.
#' @seealso \code{\link{computeOptimalPrice}} \code{\link{computeProfit}}
#' @examples 
#' 
#' network <- new("WoMNetwork", size = 1000, avgConnections = 5)
#' campaign <- new("WoMCampaign", network = network, seedingSize = 10, forwardProbability = 0.2)
#' threshold <- computeInformationCostsThreshold(campaign)
#' print(threshold)
#' 
#' @export
computeInformationCostsThreshold = function(campaign) {
    price = computeOptimalPrice(campaign)
    threshold = campaign@network@size * (price - price^2 - 0.25) / (campaign@seedingSize - campaign@network@size)
    return(threshold)
}


#' Compares the welfare of the WoM campaign to that of a fully informed market
#' 
#' Compares the welfare of the WoM campaign to the welfare of a fully informed market assuming a uniformly distributed willingness to pay.
#' 
#' @param campaign Word-of-Mouth campaign as instance of class \code{WoMCampaign}.
#' @author Michael Scholz \email{michael.scholz@@th-deg.de}
#' @author Thomas Woehner \email{Thomas.Woehner@@eah-jena.de}
#' @author Ralf Peters \email{ralf.peters@@wiwi.uni-halle.de}
#' @return Data frame containing the profit-maximizing price, the resulting demand, profit, consumer surplus and 
#' economic welfare for a fully informed market and a WoM market.
#' @seealso \code{\link{computeOptimalPrice}} \code{\link{computeProfit}} \code{\link{computeConsumerSurplus}}
#' @examples 
#' 
#' network <- new("WoMNetwork", size = 1000, avgConnections = 5)
#' campaign <- new("WoMCampaign", network = network, seedingSize = 10, forwardProbability = 0.2)
#' comparison <- compareToFIMarket(campaign)
#' print(comparison)
#' 
#' @export
compareToFIMarket = function(campaign) {
    pWoM = computeOptimalPrice(campaign)
    dWoM = computeDemand(campaign, pWoM)
    prWoM = computeProfit(campaign, pWoM)
    csWoM = computeConsumerSurplus(campaign, pWoM)
    ewWoM = prWoM + csWoM
    pInf = 0.5
    dInf = (1 - pInf) * campaign@network@size
    ci = ifelse(.hasSlot(campaign, "informationCosts") && length(campaign@informationCosts) > 0, 
                campaign@informationCosts, 0)
    prInf = dInf * pInf - campaign@network@size * ci
    csInf = campaign@network@size * (1 - pInf)^2 / 2
    ewInf = prInf + csInf
    result = data.frame(market = c("Fully Informed Market", "WoM Market"),
                        price = c(pInf, pWoM),
                        demand = c(dInf, dWoM),
                        profit = c(prInf, prWoM),
                        consumerSurplus = c(csInf, csWoM),
                        welfare = c(ewInf, ewWoM))
    return(result)
}

#' Calculates the impact of incentivizing WoM communication
#' 
#' Calculates the impact of incentivizing WoM communication. Given a start forward probability and an expected end forward, 
#' probability this function calculates changes in demand, consumer surplus, profit, cost for incentivizing and economic welfare
#' for i) keeping the optimal price for the start forward probability or ii) setting the optimized price for the expected 
#' forward probability.
#' 
#' @param campaign Word-of-Mouth campaign as instance of class \code{WoMCampaign}.
#' @param expProb Expected forward probability when incentivizing WoM.
#' @param rewardCost Cost per consumer acquired through the incentivization strategy.
#' @param keepStartPrice Logical value indicating whether or not (default) the optimized price for the start forward probability will also used for the expected forward probability.
#' @author Michael Scholz \email{michael.scholz@@th-deg.de}
#' @author Thomas Woehner \email{Thomas.Woehner@@eah-jena.de}
#' @author Ralf Peters \email{ralf.peters@@wiwi.uni-halle.de}
#' @return Data frame containing the profit-maximizing price, the resulting demand, profit, consumer surplus and 
#' economic welfare for the start WoM intensity and the expected WoM intensity.
#' @seealso \code{\link{computeOptimalPrice}} \code{\link{computeProfit}} \code{\link{computeConsumerSurplus}}
#' @examples 
#' 
#' network <- new("WoMNetwork", size = 1000, avgConnections = 5)
#' campaign <- new("WoMCampaign", network = network, seedingSize = 10, forwardProbability = 0.2)
#' incentivization <- incentivizeWoM(campaign = campaign, expProb = 0.25, rewardCost = 0.05)
#' print(incentivization)
#' 
#' @export
incentivizeWoM = function(campaign, expProb, rewardCost = 0, keepStartPrice = FALSE) {
    p1 = computeOptimalPrice(campaign)
    d1 = computeDemand(campaign, p1)
    pr1 = computeProfit(campaign, p1)
    cs1 = computeConsumerSurplus(campaign, p1)
    ew1 = pr1 + cs1
    int1 = computeWoMIntensity(campaign)
    
    campaign2 = campaign
    campaign2@forwardProbability = expProb
    if (keepStartPrice) {
        p2 = p1
    } else {
        p2 = computeOptimalPrice(campaign2)
    }
    d2 = computeDemand(campaign2, p2)
    pr2 = computeProfit(campaign2, p2)
    cs2 = computeConsumerSurplus(campaign2, p2)
    ew2 = pr2 + cs2
    int2 = computeWoMIntensity(campaign2)
    
    incCost = (d2 - d1) * rewardCost
    result = data.frame(
        forwardProbability = c(campaign@forwardProbability, expProb),
        womIntensity = c(int1, int2),
        price = c(p1, p2),
        demand = c(d1, d2),
        profit = c(pr1, pr2),
        consumerSurplus = c(cs1, cs2),
        welfare = c(ew1, ew2),
        cost = c(0, incCost)
    )
    return(result)
}

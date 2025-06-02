#' Class \code{WoMNetwork}
#' 
#' This class represents an average random graph.
#'
#' @name WoMNetwork-class
#' @aliases WoMNetwork-class
#' @docType class
#' @slot size (numeric) The number of consumers in the network.
#' @slot avgConnections (numeric) Average number of connections per consumer.
#' @author Michael Scholz \email{michael.scholz@@th-deg.de}
#' @author Thomas Woehner \email{Thomas.Woehner@@eah-jena.de}
#' @author Ralf Peters \email{ralf.peters@@wiwi.uni-halle.de}
#' @keywords classes
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("WoMNetwork", ...)}. This S4 class describes \code{WoMNetwork} objects.
#' @examples 
#' 
#' network <- new("WoMNetwork", size = 1000, avgConnections = 5)
#' print(network)
#' 
#' @export
setClass(
    "WoMNetwork",
    representation(
        size = "numeric",
        avgConnections = "numeric"
    )
)

#' Class \code{WoMCampaign}
#' 
#' This class represents a WoM campaign that is performed on a given network to promote a durable good with no variable costs.
#' 
#' @name WoMCampaign-class
#' @aliases WoMCampaign-class
#' @docType class
#' @slot network (WoMNetwork) The network to which the WoM campaign is applied.
#' @slot seedingSize (numeric) Number of consumers who are initially informed about the good by the firm.
#' @slot forwardProbability (numeric) Probability at which a consumer forwards information about the good to others.
#' @slot informationCosts (numeric) Costs to information one consumer about the good.
#' @author Michael Scholz \email{michael.scholz@@th-deg.de}
#' @author Thomas Woehner \email{Thomas.Woehner@@eah-jena.de}
#' @author Ralf Peters \email{ralf.peters@@wiwi.uni-halle.de}
#' @keywords classes
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("WoMCampaign", ...)}. This S4 class describes \code{WoMNetwork} objects.
#' @examples 
#' 
#' network <- new("WoMNetwork", size = 1000, avgConnections = 5)
#' campaign <- new("WoMCampaign", network = network, seedingSize = 10, forwardProbability = 0.2)
#' print(campaign)
#' 
#' @export
setClass(
    "WoMCampaign",
    representation(
        network = "WoMNetwork",
        seedingSize = "numeric",
        forwardProbability = "numeric",
        informationCosts = "numeric"
    )
)

#' Shows a \code{WoMNetwork} object
#' 
#' @docType methods
#' @rdname WoMNetwork-method
#' @param object An instance of the \code{WoMNetwork}-class
#' @section Methods: \describe{
#' \item{list("signature(object = \"WoMNetwork\")")}{Shows an \code{WoMNetwork} object.}}
#' @author Michael Scholz \email{michael.scholz@@th-deg.de}
#' @author Thomas Woehner \email{Thomas.Woehner@@eah-jena.de}
#' @author Ralf Peters \email{ralf.peters@@wiwi.uni-halle.de}
#' @keywords methods
#' @export
setMethod("show", "WoMNetwork", function(object) {
    cat("Word-of-Mouth network with", object@size, "persons.\n")
    cat("Each person has", object@avgConnections, "friends on average.\n")
})

#' Shows a \code{WoMCampaign} object
#' 
#' @docType methods
#' @rdname WoMCampaign-method
#' @param object An instance of the \code{WoMCampaign}-class
#' @section Methods: \describe{
#' \item{list("signature(object = \"WoMCampaign\")")}{Shows an \code{WoMCampaign} object.}}
#' @author Michael Scholz \email{michael.scholz@@th-deg.de}
#' @author Thomas Woehner \email{Thomas.Woehner@@eah-jena.de}
#' @author Ralf Peters \email{ralf.peters@@wiwi.uni-halle.de}
#' @keywords methods
#' @export
setMethod("show", "WoMCampaign", function(object) {
    cat("Word-of-Mouth campaign with", object@seedingSize, "initially infected persons.\n")
    cat("Each person forwards information to other persons with a probability of", object@forwardProbability*100, "%.\n")
})
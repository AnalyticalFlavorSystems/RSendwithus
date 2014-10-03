require(jsonlite)
require(httr)

#' Send an email with template
#'
#' This sends an email with template
#'
#' @param api_key api_key for this
#'
#' @param template_id The id for the template you want to send
#'
#' @param variables This is a list containing variables with the variable name as the list item name
#'
#' @param recipient This is a list containing the following params
#' \describe{
#'    \item{name}{The name of the person}
#'    \item{address}{The address of the person}
#' }
#'
#' @param sender This is a list containing the following params
#' \describe{
#'    \item{name}{The name of the company/person sending}
#'    \item{address}{The address of the company/person sending}
#'    \item{reply_to}{The address of the company/person to reply to.}
#' }
#' 
#' @param esp The id of the ESP to send through
#'
#' @param cc A vector containing cc emails
#'
#' @param bcc A vector containing bcc emails
#'
#' @param tags A vector containing tags 
#'
#' @param inline A list containing the following 
#' \describe{
#'    \item{id}{The id of the file, e.g 'cat.png'},
#'    \item{data}{base_64_encoded_file_data}
#' }
#'
#' @param files A data frame containing the following 
#' \describe{
#'    \item{id}{The id of the file, e.g 'cat.png'},
#'    \item{data}{base_64_encoded_file_data}
#' }
#'
#' @return json with status equal to ok 
#'
#' @export
#'
sendwithus_send <- function(api_key, template_id, variables, recipient, sender, esp=NULL,
                            cc=NULL, bcc=NULL, tags=NULL, inline=NULL, files=NULL) {
  sendData <- list(email_id=template_id, sender=sender, 
                   recipient=recipient, email_data=variables, esp_account=esp,
                   files=files, inline=inline,tags=data.frame(tags),
                   cc=data.frame(address=cc), bcc=data.frame(address=bcc))
  link <- "https://api.sendwithus.com/api/v1/send"
  jsonData <- toJSON(sendData, auto_unbox=TRUE, pretty=TRUE)
  .post(api_key, link, jsonData)
}

#' Get an list of templates 
#'
#' This returns a list of templates in sendwithus
#'
#' @param api_key Your Stripe API Key
#'
#' @return A list of templates
#'
#' @export
#'
sendwithus_list_templates <- function(api_key) {
  link <- "https://api.sendwithus.com/api/v1/templates"
  .get(api_key, link)
}


.post <- function(api_key, link, args) {
    res <- POST(url = link, user_agent="curl", config=authenticate(api_key, "", "basic"),
                body=args, encode="form")
    content(res)
}

.get <- function(api_key, link) {
    res <- GET(url=link, user_agent="curl", config=authenticate(api_key, "", "basic"))
    content(res)
}

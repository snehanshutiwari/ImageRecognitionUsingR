#file.edit(file.path("~", ".Rprofile"))


# download.file("https://media.giphy.com/media/11abbfUR5Md6YE/giphy.gif",destfile = "giphy.gif")

library(magick)

library(magick)               # library for image manipulation
library(googleAuthR)          # library for authorizing Google cloud access
library(RoogleVision)         # library for Google Vision API calls
library(tidyverse)            #
library(RCurl)




## Setup the google authentication
## client id and secret id is stored in the environe file
options("googleAuthR.client_id" = Sys.getenv("google_client_id"))
options("googleAuthR.client_secret" = Sys.getenv("google_secret.id"))

options(
  "googleAuthR.scopes.selected" = c("https://www.googleapis.com/auth/cloud-platform")
)
googleAuthR::gar_auth()


#
# PicVisionStatsLabels = getGoogleVisionResponse("https://78.media.tumblr.com/554760cdbb113582267efcb0ed024f64/tumblr_oyah4aLVCr1rpubqio2_400.gif",feature = 'LABEL_DETECTION',numResults = 20)
#
# PicVisionStatsLabels


## Read the Image
img <- image_read("./ExplicitImages/giphy.gif")

print(img)

list.all <- vector(mode = "list", length = length(img))


#### Each frome of the gif will be stored as separate jpeg image
#### And the we will cal google vision for each image separately
for (i in seq(length(img))) {
  image_name <- paste("./ExplicitImages/gif", i, ".jpeg", sep = "")
  
  image_write(img[i], path = image_name, format = "jpeg")
  
  PicVisionStatsFace = getGoogleVisionResponse(image_name, feature = 'FACE_DETECTION', numResults = 40)
  
  ## Generating boundry for the face
  xs1 <- PicVisionStatsFace$fdBoundingPoly$vertices[[1]][1][[1]]
  ys1 <- PicVisionStatsFace$fdBoundingPoly$vertices[[1]][2][[1]]
  
  ## Generating landmarks
  xs2 = PicVisionStatsFace$landmarks[[1]][[2]][[1]]
  ys2 = PicVisionStatsFace$landmarks[[1]][[2]][[2]]
  #
  #
  list.all[[i]] <- list(xs1, ys1, xs2, ys2)
}

### Recreate the gif from the frames --- Not a good way to do it.
list.all.image <- vector(mode = "list", length = length(img))
list.all.image_c <- vector(mode = "character")
for (i in seq(length(list.all))) {
  image_name <- paste("./ExplicitImages/gif", i, ".jpeg", sep = "")
  img <- image_read(image_name)
  new_img <- image_draw(img)
  rect(
    unlist(list.all[[i]][1])[1],
    unlist(list.all[[i]][2])[1],
    unlist(list.all[[i]][1])[3],
    unlist(list.all[[i]][2])[3],
    border = "red",
    lty = "dashed",
    lwd = 1
  )
  points(unlist(list.all[[i]][3]),
         unlist(list.all[[i]][4]),
         col = "red",
         cex = 0.9)
  
  dev.off()
  if (i == 1) {
    list.all.image_c <- new_img
  } else
  {
    list.all.image_c <- c(list.all.image_c, new_img)
  }
}


## Morph all the images together with a frame rate
frames <- image_morph(list.all.image_c, frames = .5)

## Animate the gif
image_animate(frames)


#### Ignore forllowing lines
#
#
# print(list.all.image[[8]])
#
# unlist(list.all[[5]][5])[1]
# xs1[1]
#
# new_img <- image_draw(img)
# rect(xs1[1],ys1[1],xs1[3],ys1[3],border = "red", lty = "dashed", lwd = 1)
# points(xs2,ys2,col="red",cex=0.9)
#
# dev.off()
#
# class(new_img8)
#
# list(new_img8)
#
# frames <- image_morph(c(new_img1, new_img2,new_img3,new_img4,new_img5,new_img6,new_img7,new_img8), frames = 0.0025)
#
#
# image_animate(frames)
#
#
#
# ## Get face detection from getgooglevision
# PicVisionStatsFaceNew1 = getGoogleVisionResponse(image_name,feature = 'FACE_DETECTION',numResults = 40)
#
# PicVisionStatsFaceNew2 = getGoogleVisionResponse("./ExplicitImages/gif2.jpeg",feature = 'FACE_DETECTION',numResults = 40)
#
# PicVisionStatsFaceNew3 = getGoogleVisionResponse("./ExplicitImages/gif3.jpeg",feature = 'FACE_DETECTION',numResults = 40)
#
# PicVisionStatsFaceNew4 = getGoogleVisionResponse("./ExplicitImages/gif4.jpeg",feature = 'FACE_DETECTION',numResults = 40)
#
# PicVisionStatsFaceNew5 = getGoogleVisionResponse("./ExplicitImages/gif5.jpeg",feature = 'FACE_DETECTION',numResults = 40)
#
# PicVisionStatsFaceNew6 = getGoogleVisionResponse("./ExplicitImages/gif6.jpeg",feature = 'FACE_DETECTION',numResults = 40)
#
# PicVisionStatsFaceNew7 = getGoogleVisionResponse("./ExplicitImages/gif7.jpeg",feature = 'FACE_DETECTION',numResults = 40)
#
# PicVisionStatsFaceNew8 = getGoogleVisionResponse("./ExplicitImages/gif7.jpeg",feature = 'FACE_DETECTION',numResults = 40)
#
#
# #
# # PicVisionStatsSafe = getGoogleVisionResponse("./ExplicitImages/giphy.gif",feature = 'SAFE_SEARCH_DETECTION',numResults = 20)
# #
# # PicVisionStatsFace
#
# ## Generating boundry for the face
# xs1 <- PicVisionStatsFaceNew8$fdBoundingPoly$vertices[[1]][1][[1]]
# ys1 <- PicVisionStatsFaceNew8$fdBoundingPoly$vertices[[1]][2][[1]]
#
# ## Generating landmarks
# xs2 = PicVisionStatsFaceNew8$landmarks[[1]][[2]][[1]]
# ys2 = PicVisionStatsFaceNew8$landmarks[[1]][[2]][[2]]
# #
# # img <- image_read("https://78.media.tumblr.com/554760cdbb113582267efcb0ed024f64/tumblr_oyah4aLVCr1rpubqio2_400.gif")
#
# list8 <- list(xs1,ys1,xs2,ys2,PicVisionStatsFaceNew8)
#
# img <- image_read("./ExplicitImages/gif8.jpeg")
#
# new_img8 <- image_draw(img)
# rect(xs1[1],ys1[1],xs1[3],ys1[3],border = "red", lty = "dashed", lwd = 1)
# points(xs2,ys2,col="red",cex=0.9)
#
# dev.off()
#
# print(new_img1)
#
# frames <- image_morph(c(new_img1, new_img2,new_img3,new_img4,new_img5,new_img6,new_img7,new_img8), frames = 0.0025)
#
#
# image_animate(frames)
#
# ################################# Safe Search Detection Example###################
# ### encode image
# imageFile <- "./ExplicitImages/giphy.gif"
# txt <- base64Encode(readBin(imageFile, "raw", file.info(imageFile)[1, "size"]), "txt")
# ### create Request, following the API Docs.
# body= paste0('{  "requests": [    {   "image": { "content": "',txt,'" }, "features": [  { "type": "SAFE_SEARCH_DETECTION", "maxResults": 2} ],  }    ],}')
# ## generate function call
# simpleCall <- gar_api_generator(baseURI = "https://vision.googleapis.com/v1/images:annotate", http_header="POST" )
# ## set the request!
# pp <- simpleCall(the_body = body)
# ## obtain results.
# pp$content$responses$safeSearchAnnotation
# ####################################################################################
#
#

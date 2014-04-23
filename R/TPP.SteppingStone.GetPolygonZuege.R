#' @name TPP.SteppingStone.GetPolygonZuege
#' @rdname TPP.SteppingStone.GetPolygonZuege 
#' @title Transportation-Problem -- Stepping-Stone-Method -- Helper: GetPolygonZuege
#'
#' @description Calculates all Polygon-Cycles.
#' @section WARNING:
#'  This function is meant for internal purposes only. It is not required to call this function manually at any time!
#' @param object Object of Type \code{\link{GeoSituation}}
#' @param i_i integer REQUIRED. Describes the pivot-Element the polygon-cycles should be searched for.
#' @param j_j integer REQUIRED. Describes the pivot-Element the polygon-cycles should be searched for.
#' @param ... \emph{Optional Parameters} See Below.
#'     
#' @section Optional Parameters (\code{...}): 
#' \subsection{used by \code{\link{TPP.SteppingStone.GetPolygonZuege}}}{
#'    \describe{ 
#' 		\item{log}{\code{"logical"} Optional Parameter. Indicating, if the calculation should be logged to console. Default is \code{FALSE}.}  
#' 		\item{Poly}{\code{"list"} \emph{Optional Parameter}. Used only if the function is called recursively.}
#'    } 
#' }
#' \subsection{Forwarded to the follwowing functions}{  
#'    You may want to check these functions for any other optional parameters.
#'    \itemize{
#'      \item{\code{\link{TPP.SteppingStone.CalcPolyGonZug}}}
#'      \item{\code{\link{TPP.SteppingStone.GetPolygonZuege}}}
#'    }
#' }  
#' @keywords OR Transportation-Problem TPP Stepping-Stone
#' @details Explain what SSM does.
#' @export  
#' @references Domschke
#' @seealso \code{\link{GeoSituation}}, \code{\link{Node}}, \code{\link{TPP.NW}}, \code{\link{TPP.CMM}}, \code{\link{TPP.MMM}}, \code{\link{TPP.SteppingStone}}, \code{\link{TPP.MODI}}
#' @note 
#'      for citing use: Felix Lindemann (2014). HNUORTools: Operations Research Tools. R package version 1.1-0. \url{http://felixlindemann.github.io/HNUORTools/}.
#'      
#' @author Dipl. Kfm. Felix Lindemann \email{felix.lindemann@@hs-neu-ulm.de} 
#' 
#' Wissenschaftlicher Mitarbeiter
#' Kompetenzzentrum Logistik
#' Buro ZWEI, 17
#'
#' Hochschule fur angewandte Wissenschaften 
#' Fachhochschule Neu-Ulm | Neu-Ulm University 
#' Wileystr. 1 
#' 
#' D-89231 Neu-Ulm 
#' 
#' 
#' Phone   +49(0)731-9762-1437 
#' Web      \url{www.hs-neu-ulm.de/felix-lindemann/} 
#'			\url{http://felixlindemann.blogspot.de}
setGeneric("TPP.SteppingStone.GetPolygonZuege",  function(object,i_i, j_j, ...)  standardGeneric("TPP.SteppingStone.GetPolygonZuege") )

#' @aliases TPP.SteppingStone.GetPolygonZuege,GeoSituation,integer,integer-method
#' @rdname TPP.SteppingStone.GetPolygonZuege
setMethod("TPP.SteppingStone.GetPolygonZuege", signature(object="GeoSituation", i_i = "integer", j_j = "integer"),
          function(object,i_i, j_j,...){ 
            
            li <- list(...)  
            result<-NULL  #Rueckgabe 
            if(is.null(li$log)) li$log <- TRUE 
            
            
            cij <- object$tpp$cij
            xij <- object$tpp$x
            
            
            I<-nrow(cij)
            J<-ncol(cij)
            if(is.null(li$Poly)){
              #Ersten PolygonZug initialisieren
              n.Poly<-NULL
              n.Poly$List<-data.frame(i=i_i,j=j_j)
              n.Poly$opp<-cij[i_i,j_j]
              n.Poly$vertical<-TRUE 
              n.Poly$closed<-FALSE 
              n.Poly$NextIsAdd<-FALSE
              n.Poly$count<-1
              if(xij[i_i,j_j] >0){
                n.Poly$closed=TRUE
              }
              Poly <- n.Poly 
            }else{
              # UEbergebener Polygonzug
              Poly<-li$Poly
            }  
            
            
            if (Poly$closed){
              # n.Polies <- rbind(n.Polies,data.frame(PZ=Poly))  
              result <- rbind(result,Poly)  
            }else{ 
              if (Poly$vertical){
                #Suche in vertikaler Richtung
                j<-Poly$List$j[length(Poly$List$j)] 
                if (length(Poly$List$j)>1 & j==Poly$List$j[1]){
                  #Polygonzug Finalisieren
                  Poly$closed<-TRUE
                  result<-rbind(result,Poly)
                }else{
                  #Noch nicht belegte Zeilen Holen
                  Y<-1:I 
                  Y<-Y[-Poly$List$i] 
                  Y<-cbind(Y,Poly$List$i[1]) 
                  #... und ueber diese Iterieren 
                  for (i in Y){
                    # temp<-paste("x[",i,",",j,"] = ",xij[i,j])
                    # print(temp) 
                    n.Poly<-TPP.SteppingStone.CalcPolyGonZug(object,i=i,j=j,Poly=Poly)
                    if (!is.null(n.Poly)){
                      result<-rbind(result,n.Poly)
                    }
                  }
                }
              }else{
                #Suche in horizontaler Richtung
                i<-Poly$List$i[length(Poly$List$i)]  
                if (length(Poly$List$i)>1 & i==Poly$List$i[1]){
                  #Polygonzug Finalisieren
                  Poly$closed<-TRUE
                  result<-rbind(result,Poly)
                }else{
                  #Noch nicht belegte Spalten Holen
                  X<-1:J
                  X<-X[-Poly$List$j]
                  X<-cbind(X,Poly$List$j[1]) 
                  
                  #... und ueber diese Iterieren
                  for (j in X){ 
                    # temp<-paste("x[",i,",",j,"] = ",xij[i,j])
                    # print(temp)   
                    n.Poly<-TPP.SteppingStone.CalcPolyGonZug(object,i=i,j=j,Poly=Poly)
                    if (!is.null(n.Poly)){
                      result<-rbind(result,n.Poly)
                    }
                  }
                }
              }
            } 
            if (!is.null(result)){
              r.result<-result 
              y<-nrow(r.result)
              
              result<-NULL 
              for (k in (1:y)){
                n.Poly<-r.result[k,]  
                if(n.Poly$closed){
                  if(n.Poly$count>=4){
                    result<-rbind(result,n.Poly)
                  }
                }else{
                  n.result<-TPP.SteppingStone.GetPolygonZuege(object,i_i=i,j_j=j,Poly=n.Poly)
                  if (!is.null(n.result)){
                    Y<-nrow(n.result)
                    
                    for (l in (1:Y)){ 
                      l.Poly<-n.result[l,]
                      result<-rbind(result,l.Poly)
                    }
                  }
                }
              } 
            }
            return(result)
          }
)

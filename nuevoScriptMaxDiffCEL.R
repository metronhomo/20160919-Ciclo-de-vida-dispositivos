function (datos, nItems, nBloques, nItemsPorBloque, estructura = "gut") 
{
    celulares<-maxdiffModelo(datos = z,nItems = 20,nItemsPorBloque = 6,nBloques = 13,estructura = 'chida')
    datos <- z
    nItems <- 20
    nItemsPorBloque <- 6
    nBloques <- 13
    estructura <-  'chida'
    m_quitaceros <- function(dat) {
        dat <- dat %>% mutate_each(funs(as.character))
        dat[is.na(dat)] <- 0
        dat <- dat %>% mutate_each(funs(as.factor))
        return(dat)
    }
    m_cambio_estructura <- function(datos = datos, nBloques = nBloques, 
                                    nItemsPorBloque) {
        a <- data.frame(letters[1:nItemsPorBloque])
        b <- a
        for (i in 1:(nBloques * 2 - 1)) {
            b <- cbind(b, a)
        }
        names(b) <- paste0("b", 1:(nBloques * 2))
        b <- b[0, ]
        for (i in 1:(length(b)/2)) {
            levels(b[, (2 * (i - 1) + 1)]) <- names(datos)[(nItemsPorBloque * 
                                                                (i - 1) + 1):(nItemsPorBloque * (i - 1) + nItemsPorBloque)]
            levels(b[, (2 * (i - 1) + 2)]) <- names(datos)[(nItemsPorBloque * 
                                                                (i - 1) + 1):(nItemsPorBloque * (i - 1) + nItemsPorBloque)]
        }
        for (k in 1:nrow(datos)) {
            for (i in 1:nBloques) {
                for (j in 1:nItemsPorBloque) {
                    if (datos[k, (i - 1) * nItemsPorBloque + j] == 
                        "Más importante") {
                        b[k, (i - 1) * 2 + 1] <- names(datos)[(i - 
                                                                   1) * nItemsPorBloque + j]
                    }
                    if (datos[k, (i - 1) * nItemsPorBloque + j] == 
                        "Menos importante") {
                        b[k, (i - 1) * 2 + 2] <- names(datos)[(i - 
                                                                   1) * nItemsPorBloque + j]
                    }
                }
            }
        }
        return(b)
    }
    m_obtiene_atributos <- function(datos) {
        niveles <- NULL
        for (i in 1:length(datos)) {
            niveles <- c(niveles, unique(levels(datos[, i])))
        }
        niveles <- unique(niveles)
        niveles <- na.omit(niveles)
        niveles <- as.character(niveles)
        return(niveles)
    }
    m_llena_bloque_z <- function(bloque = 1, nBloques = nBloques, 
                                 nombresItems = nombresItems, matriz = datos) {
        nAtributos <- length(nombresItems)
        z <- matrix(nrow = 1, ncol = nAtributos)
        z <- as.data.frame(z)
        k <- 1
        for (j in 1:nAtributos) {
            names(z)[k] <- paste0("b", bloque, "v", j)
            k <- k + 1
        }
        z <- z %>% mutate_each(funs(as.numeric))
        matcheo <- match(levels(matriz[, (1 + (2 * (bloque - 
                                                        1)))]), nombresItems)
        for (i in 1:nrow(matriz)) {
            z[i, matcheo] <- 0
            matcheoMejor <- match(matriz[i, (1 + (2 * (bloque - 
                                                           1)))], nombresItems)
            z[i, matcheoMejor] <- 1
            matcheoPeor <- match(matriz[i, (2 + (2 * (bloque - 
                                                          1)))], nombresItems)
            z[i, matcheoPeor] <- (-1)
        }
        return(z)
    }
    if (estructura == "fuchi") {
        mNombresCambio <- names(datos)
        names(datos) <- paste0("v", 1:length(datos))
        datos <- m_quitaceros(datos)
        names(datos) <- mNombresCambio
        datos <- m_cambio_estructura(datos, nBloques = nBloques, 
                                     nItemsPorBloque = nItemsPorBloque)
    }
    datos <- na.omit(datos)
    nombresItems <- m_obtiene_atributos(datos)
    bloques <- list()
    for (i in 1:nBloques) {
        bloques[[i]] <- m_llena_bloque_z(bloque = i, nBloques = nBloques, 
                                         nombresItems = nombresItems, matriz = datos)
    }
    z <- do.call("cbind", bloques)
    n = nrow(z)
    nObservaciones = n * nBloques
    datosMaxDiff = matrix(as.numeric(t(z)), ncol = nItems, byrow = TRUE, 
                          dimnames = list(1:nObservaciones, nombresItems))
    conteos = apply(datosMaxDiff, 2, mean, na.rm = TRUE)
    rankings = nItems + 1 - rank(conteos)
    globalR <- cbind(scoreGeneral = conteos, rankingGeneral = rankings)
    globalR
    id = rep(1:n, rep(nBloques, n))
    conteosIndividuales = aggregate(datosMaxDiff, list(id), mean, 
                                    na.rm = TRUE)[, -1]
    set.seed(14561)
    conteosIndividualesSinEmpates = conteosIndividuales + matrix(runif(n * 
                                                                           nItems)/1e+05, n)
    rankings = nItems + 1 - apply(conteosIndividualesSinEmpates, 
                                  1, rank)
    sucio <- apply(rankings, 1, table)
    if (is.list(sucio)) {
        limpio <- list()
        for (i in 1:length(sucio)) {
            completo <- as.character(1:nItems)
            miniLimpio <- rep(0, nItems)
            llave <- match(completo, names(sucio[[i]]))
            miniLimpio <- sucio[[i]][llave]
            limpio[[i]] <- miniLimpio
        }
        limpio <- do.call("cbind", limpio)
        rownames(limpio) <- 1:nItems
        limpio[is.na(limpio)] <- 0
    }
    if (!is.list(sucio)) {
        limpio <- sucio
    }
    proporcionesRankings <- t(limpio/n * 100)
    proporcionesAcumulativasRankings = t(apply(proporcionesRankings, 
                                               1, cumsum))
    rankingsPromedio = proporcionesRankings %*% (1:nItems)/100
    rankings = rank(rankingsPromedio)
    individualR <- cbind(scoreIndividual = as.numeric(rankingsPromedio), 
                         rankingIndividual = rankings)
    individualR
    rownames(individualR) <- nombresItems
    nRows = nItemsPorBloque * 2 * nBloques * n
    datosLargos = matrix(0, nRows, nItems + 3)
    counter = 0
    setCounter = 0
    for (rr in 1:nObservaciones) {
        nAlts = 0
        alternatives = NULL
        respondent = floor(rr/nBloques) + 1
        for (cc in 1:nItems) {
            v = datosMaxDiff[rr, cc]
            if (!is.na(v)) {
                nAlts = nAlts + 1
                alternatives[nAlts] = cc
                if (v == 1) 
                    best = cc
                if (v == -1) 
                    worst = cc
            }
        }
        setCounter = setCounter + 1
        for (a in 1:nAlts) {
            counter = counter + 1
            this_a = alternatives[a]
            if (this_a == best) 
                datosLargos[counter, 3] = 1
            else if (this_a == worst) 
                datosLargos[counter + nAlts, 3] = 1
            datosLargos[counter, 1] = respondent
            datosLargos[counter + nAlts, 1] = respondent
            datosLargos[counter, 2] = setCounter
            datosLargos[counter + nAlts, 2] = setCounter + 1
            datosLargos[counter, 3 + this_a] = 1
            datosLargos[counter + nAlts, 3 + this_a] = -1
        }
        setCounter = setCounter + 1
        counter = counter + nAlts
    }
    datosLargos = as.data.frame(datosLargos)
    nombres <- paste0("v", 1:nItems)
    names(datosLargos) <- c("ID", "Set", "Eleccion", nombres)
    reemplazo <- datosLargos$ID[datosLargos$ID <= nrow(datos)]
    datosLargos$ID <- c(rep(1, nItemsPorBloque * 2), reemplazo)
    formula <- paste0("v", 2:nItems, collapse = "+")
    eval(parse(text = paste0("logitModel = mlogit(Eleccion ~ ", 
                             formula, " | 0, \n                    data = datosLargos, alt.levels = paste(1:nItemsPorBloque), \n                    shape = \"long\")")))
    summary(logitModel)
    logitR <- cbind(scoreLogit = c(0, as.numeric(logitModel$coefficients)), 
                    rankingLogit = nItems + 1 - rank(c(0, as.numeric(logitModel$coefficients))))
    rownames(logitR) <- nombresItems
    if (sum(rownames(globalR) == rownames(individualR)) == nItems) {
        if (sum(rownames(globalR) == rownames(logitR)) == nItems) {
            turboResultado <- cbind(globalR, individualR, logitR)
        }
    }
    return(turboResultado)
}
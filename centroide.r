centroide <- function(treinamento) {
  somaC11 = 0;
  somaC12 = 0;
  somaC13 = 0;
  somaC14 = 0;
  qntC1 = 0;
  
  somaC21 = 0;
  somaC22 = 0;
  somaC23 = 0;
  somaC24 = 0;
  qntC2 = 0;
  
  somaC31 = 0;
  somaC32 = 0;
  somaC33 = 0;
  somaC34 = 0;
  qntC3 = 0;
  
  for (i in 1: nrow(treinamento)) {
    if (treinamento[i,5] == 1) {
      somaC11 = somaC11 + treinamento[i,1]
      somaC12 = somaC12 + treinamento[i,2]
      somaC13 = somaC13 + treinamento[i,3]
      somaC14 = somaC14 + treinamento[i,4]
      qntC1 = qntC1 + 1
    }
    else if (treinamento[i,5] == 2) {
      somaC21 = somaC21 + treinamento[i,1]
      somaC22 = somaC22 + treinamento[i,2]
      somaC23 = somaC23 + treinamento[i,3]
      somaC24 = somaC24 + treinamento[i,4]
      qntC2 = qntC2 + 1
    }
    else {
      somaC31 = somaC31 + treinamento[i,1]
      somaC32 = somaC32 + treinamento[i,2]
      somaC33 = somaC33 + treinamento[i,3]
      somaC34 = somaC34 + treinamento[i,4]
      qntC3 = qntC3 + 1
    }
  }
  centroides<-list()
  centroides[[1]] = c((somaC11 / qntC1), (somaC12 / qntC1), (somaC13 / qntC1), (somaC14 / qntC1))
  centroides[[2]] = c((somaC21 / qntC2), (somaC22 / qntC2), (somaC23 / qntC2), (somaC24 / qntC2))
  centroides[[3]] = c((somaC31 / qntC3), (somaC32 / qntC3), (somaC33 / qntC3), (somaC34 / qntC3))
  
  return (centroides)
}

verificarCentroide <- function(centroides, e) {
  distancias<-c()
  for (i in 1: 3) {
    distancias[i] = sqrt((centroides[[i]][1] - e[1])^2 + (centroides[[i]][2] - e[2])^2 + (centroides[[i]][3] - e[3])^2 + (centroides[[i]][4] - e[4])^2)
  }
  
  menor = min(distancias)
  indiceMenorDistancia = match(menor,distancias)
  
  ret = list()
  ret$e = e
  ret$classe = indiceMenorDistancia
  ret$acertou = indiceMenorDistancia == e[5]
  
  return(ret)
}

processaCentroide <- function(classe1, classe2, classe3) {
  txAcertos<-c()
  txAcertosUm<-c()
  txAcertosDois<-c()
  txAcertosTres<-c()
  
  #Laço para rodar 30vezes a base de teste
  for (j in 1:30) {
    #Embaralhando a base original
    classe1 <- classe1[sample(1:nrow(classe1)), ]
    classe2 <- classe2[sample(1:nrow(classe2)), ]
    classe3 <- classe3[sample(1:nrow(classe3)), ]

    #Quebrando o dataset em treino e teste (80/20) OBS: Buscando as linhas de forma aleotória
    library(dplyr)
    
    dataTreinoClasse1<-sample_frac(classe1, 0.80)
    dataTesteClasse1<-setdiff(classe1, dataTreinoClasse1)
    
    dataTreinoClasse2<-sample_frac(classe2, 0.80)
    dataTesteClasse2<-setdiff(classe2, dataTreinoClasse2)
    
    dataTreinoClasse3<-sample_frac(classe3, 0.80)
    dataTesteClasse3<-setdiff(classe3, dataTreinoClasse3)
    
    dataTreino <- rbind(dataTreinoClasse1, dataTreinoClasse2, dataTreinoClasse3)
    dataTeste <- rbind(dataTesteClasse1, dataTesteClasse2, dataTesteClasse3)
    
    #Variáveis para controlar os acertos do algoritmo
    qntUm = 0
    qntDois = 0
    qntTres = 0
    qntAcertosUm = 0
    qntAcertosDois = 0
    qntAcertosTres = 0
    
    #Laço para percorrer todas as linha do dataframe de teste
    for (i in 1: (nrow(dataTeste))) {
      #Recebe a linha atual do conjunto de teste
      linha = dataTeste[i,]

      #Calculando os centroides
      centroides = centroide(dataTreino)
      
      #Chamando a função que classifica a linha atual
      resultCentroide = verificarCentroide(centroides, linha)
      
      #Verificando quantas chamadas teve de cada classe
      if (resultCentroide$e[5] == 1) {
        qntUm = qntUm + 1
      }
      else if (resultCentroide$e[5] == 2) {
        qntDois = qntDois + 1
      }
      else {
        qntTres = qntTres + 1
      }
      
      #Verificando se o algoritmo acertou na classificação
      if (resultCentroide$acertou) {
        if(resultCentroide$classe == 1) {
          qntAcertosUm = qntAcertosUm + 1
        }
        else if(resultCentroide$classe == 2) {
          qntAcertosDois = qntAcertosDois + 1
        }
        else {
          qntAcertosTres = qntAcertosTres + 1
        }
      }
    }
    
    #Calculando a taxa de acertos (Numero de acertos total / Quantidade de elementos testados)
    txAcerto = (qntAcertosUm + qntAcertosDois + qntAcertosTres) / (qntUm + qntDois + qntTres)
    
    #Calculando a taxa de acertos para classe um (Numero de acertos um / Quantidade de elementos testados para um)
    txAcertoUm = qntAcertosUm / qntUm
    
    #Calculando a taxa de acertos para classe dois (Numero de acertos dois / Quantidade de elementos testados para dois)
    txAcertoDois = qntAcertosDois / qntDois
    
    #Calculando a taxa de acertos para classe tres (Numero de acertos tres / Quantidade de elementos testados para tres)
    txAcertoTres = qntAcertosTres / qntTres
    
    #r = list()
    #r$txAcerto = txAcerto
    #r$txAcertoUm = txAcertoUm
    #r$txAcertoDois = txAcertoDois
    #r$txAcertoTres = txAcertoTres
    #r$qntUm = qntUm
    #r$qntDois = qntDois
    #r$qntTres = qntTres
    #r$qntAcertosUm = qntAcertosUm
    #r$qntAcertosDois = qntAcertosDois
    #r$qntAcertosTres = qntAcertosTres
    
    #return(r)
    
    txAcertos[j]<-txAcerto
    txAcertosUm[j]<-txAcertoUm
    txAcertosDois[j]<-txAcertoDois
    txAcertosTres[j]<-txAcertoTres
  }
  
  resultado = list()
  
  resultado$txAcertosMin = min(txAcertos)  
  resultado$txAcertosMax = max(txAcertos)
  resultado$txAcertosMed = median(txAcertos)
  resultado$txAcertosUmMed = median(txAcertosUm)
  resultado$txAcertosDoisMed = median(txAcertosDois)
  resultado$txAcertostresMed = median(txAcertosTres)
  
  return (resultado)
}

processaCentroide2 <- function(dataset) {
  txMedAcertos<-c()
  testes<-c()
  
  #Laço para alterar as proporções de 1 em 1 de 20 até 80
  for (k in 1:61) {
    txAcertos<-c()
    
    #Laço para rodar 30vezes a base de teste
    for (j in 1:30) {
      #Embaralhando a base original
      dataset <- dataset[sample(1:nrow(dataset)), ]
      
      #Quebrando o dataset em treino e teste (80/20) OBS: Buscando as linhas de forma aleotória
      library(dplyr)
      dataTreino<-sample_frac(dataset, 0.80)
      dataTeste<-setdiff(dataset, dataTreino)
      
      #Variável para controlar os acertos do algoritmo
      qntAcertosTotal = 0
      
      #Laço para percorrer todas as linha do dataframe de teste
      for (i in 1: (nrow(dataTeste))) {
        #Recebe a linha atual do conjunto de teste
        linha = dataTeste[i,]
        
        #Calculando os centroides
        centroides = centroide(dataTreino)
        
        #Chamando a função que classifica a linha atual
        resultCentroide = verificarCentroide(centroides, linha)
        
        #Verificando se o algoritmo acertou na classificação
        if (resultCentroide$acertou) {
          qntAcertosTotal = qntAcertosTotal + 1
        }
      }
      
      #Calculando a taxa de acertos (Numero de acertos total / Quantidade de elementos testados)
      txAcerto = qntAcertosTotal / nrow(dataTeste)
      
      txAcertos[j]<-txAcerto
    }
    
    txMedAcertos[k]<-median(txAcertos)
    testes[k]<-(0.19 + (k/100))
  }
  
  resultado = list()
  
  resultado$txMedAcertos = txMedAcertos 
  resultado$testes = testes
  
  return (plot(resultado$txMedAcertos~resultado$testes))
}
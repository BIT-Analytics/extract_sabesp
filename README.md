# extract_sabesp

# Info IAS

Dashboard de informações resumidas sobre os sistemas de abastecimento de água da Região Metropolitana de São Paulo.

## 📋 Sobre o Projeto

Desenvolvido pela **BIT Analytics** para o **Instituto Água e Saneamento (IAS)**, este dashboard monitora e analisa os padrões históricos dos sistemas de abastecimento, pluviometria e vazões dos mananciais de São Paulo.

## 🎯 Objetivo

Analisar os padrões históricos dos níveis dos sistemas, pluviometria e vazões para antecipar tendências e compreender o comportamento dos recursos hídricos na RMSP.

## 📊 Dados Monitorados

- **Níveis dos reservatórios** (% volume)
- **Pluviometria** e comparação com médias históricas
- **Vazão natural** das bacias hidrográficas
- **Vazão de captação** das ETAs
- **Transferência** entre sistemas
- **Médias históricas** mensais

## 🗂️ Estrutura de Dados

| Variável | Descrição |
|----------|-----------|
| `Data` | Período no formato mês/ano |
| `Sistema` | Nome do sistema de abastecimento |
| `Volume_Diario` | Nível da represa no final do mês (%) |
| `Vazao_Natural` | Vazão natural mensal e média histórica |
| `Pluviometria` | Chuva mensal (mm) e média histórica |
| `Vazao_Captada` | Vazão retirada para tratamento na ETA |
| `Transferencia` | Vazão de transferência entre sistemas |

## 🔗 Fontes

- **Portal dos Mananciais da SABESP**: https://mananciais.sabesp.com.br/
- Boletins diários de monitoramento
- Dados históricos de 2013 a 2025 (com expansão planejada)

## 🚀 Desenvolvimento

**Cliente:** Instituto Água e Saneamento (IAS)  
**Desenvolvedor:** BIT Analytics

---

*Dashboard para monitoramento estratégico dos recursos hídricos da Região Metropolitana de São Paulo*

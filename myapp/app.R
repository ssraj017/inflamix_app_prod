library(shiny)
library(shinythemes)
library(tidyverse)
library(survival)
library(ggsurvfit)
library(mvtnorm)
library(mclust)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  theme = shinytheme("united"),

  # App title ----
  titlePanel(
    img(src = "inflamix.png", height = 90, width = 500)
  ),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(
      width=6,
      tags$style(".well {background-color:#fcb79033;}"),
      h4("Instructions:", style="font-weight:bold"),
      tags$div(
        tags$ul(
          tags$li("Please enter atleast 2 lab values to generate a cluster assignment. InflaMix performance has been validated with availability of at least the first 6 labs (Hgb, Albumin, LDH, CRP, ALP, AST)"),
          tags$li("Pre-Infusion means measured within 48 hours before CAR-T infusion."),
          tags$li("Terms of Use for InflaMix are detailed at: https://github.com/vdblab/InflaMix/blob/main/LICENSE. Briefly, InflaMix is intended for educational and research use only and is not yet intended for clinical use or as a medical device."),
        )
      ),
      br(),

      fluidRow(
        column(
          6,
          h5("Hemoglobin - g/dL", style="font-weight:bold"),
          column(6,numericInput("hgb", h6("Pre-Infusion"),value = NA)),
          column(6,numericInput("hgbuln", h6("ULN"), value = 16.2))
        ),
        column(
          6,
          h5("Albumin - g/dL", style="font-weight:bold"),
          column(6,numericInput("albumin", h6("Pre-Infusion"),value = NA)),
          column(6,numericInput("albuminuln", h6("ULN"), value = 5.0))
        )
      ),

      fluidRow(
        column(
          6,
          h5("Lactate Dehydrogenase (LDH) - U/L", style="font-weight:bold"),
          column(6,numericInput("ldh", h6("Pre-Infusion"),value = NA)),
          column(6,numericInput("ldhuln", h6("ULN"), value = 250.0))
        ),
        column(
          6,
          h5("C-reactive Protein (CRP) - mg/dL", style="font-weight:bold"),
          column(6,numericInput("crp", h6("Pre-Infusion"),value = NA)),
          column(6,numericInput("crpuln", h6("ULN"), value = 0.3))
        )
      ),

      fluidRow(
        column(
          6,
          h5("Alkaline Phosphatase (ALP) - U/L", style="font-weight:bold"),
          column(6,numericInput("alp", h6("Pre-Infusion"),value = NA)),
          column(6,numericInput("alpuln", h6("ULN"), value = 130.0))
        ),
        column(
          6,
          h5("Aspartate Transaminase (AST) - U/L", style="font-weight:bold"),
          column(6,numericInput("ast", h6("Pre-Infusion"),value = NA)),
          column(6,numericInput("astuln", h6("ULN"), value = 37.0))
        )
      ),

      fluidRow(
        column(
          6,
          h5("Interleukin-6 (IL-6) - pg/mL", style="font-weight:bold"),
          column(6,numericInput("il6", h6("Pre-Infusion"),value = NA)),
          column(6,numericInput("il6uln", h6("ULN"), value = 5.0))
        ),
        column(
          6,
          h5("Interleukin-10 (IL-10) - pg/mL", style="font-weight:bold"),
          column(6,numericInput("il10", h6("Pre-Infusion"),value = NA)),
          column(6,numericInput("il10uln", h6("ULN"), value = 18.0))
        )
      ),


      fluidRow(
        column(
          6,
          h5("Tumor Necrosis Factor Alpha (TNFa) - pg/mL", style="font-weight:bold"),
          column(6,numericInput("tnfa", h6("Pre-Infusion"),value = NA)),
          column(6,numericInput("tnfauln", h6("ULN"), value = 22.0))
        ),
        column(
          6,
          h5("D-dimer - mcg/mL", style="font-weight:bold"),
          column(6,numericInput("ddimer", h6("Pre-Infusion"),value = NA)),
          column(6,numericInput("ddimeruln", h6("ULN"), value = 0.5))
        )
      ),

      fluidRow(
        column(
          6,
          h5("Ferritin - ng/mL", style="font-weight:bold"),
          column(6,numericInput("ferritin", h6("Pre-Infusion"),value = NA)),
          column(6,numericInput("ferruln", h6("ULN"), value = 415))
        ),
        column(
          6,
          h5("Total Bilirubin (Tbili) - mg/dL", style="font-weight:bold"),
          column(6,numericInput("tbili", h6("Pre-Infusion"),value = NA)),
          column(6,numericInput("tbiliuln", h6("ULN"), value = 1.2))
        )
      ),

      fluidRow(
        column(
          6,
          h5("Platelets (Plt) - K/mcL", style="font-weight:bold"),
          column(6,numericInput("plt", h6("Pre-Infusion"),value = NA)),
          column(6,numericInput("pltuln", h6("ULN"), value = 400.0))
        ),
        column(
          6,
          h5("White Blood Cells (WBC) - K/mcL", style="font-weight:bold"),
          column(6,numericInput("wbc", h6("Pre-Infusion"),value = NA)),
          column(6,numericInput("wbculn", h6("ULN"), value = 11.0))
        )
      )
    ),

    # Main panel for displaying outputs ----
    mainPanel(
      width=6,
      tabsetPanel(
        tabPanel("Risk Stratification",
                 tags$head(tags$style("#clusterprob{color: black;
                                 font-size: 20px;
                                 font-style: bold;
                                 }")
                 ),
                 tags$head(tags$style("#cluster_predcr{color: black;
                                 font-size: 20px;
                                 font-style: bold;
                                 }")
                 ),
                 tags$head(tags$style("#cluster_predpfs{color: black;
                                 font-size: 20px;
                                 font-style: bold;
                                 }")
                 ),
                 tags$head(tags$style("#cluster_predos{color: black;
                                 font-size: 20px;
                                 font-style: bold;
                                 }")
                 ),
                 br(),
                 h3("Cluster:", style="font-weight:bold"),
                 imageOutput("cluster", height="45px"),
                 br(),
                 textOutput("clusterprob"),
                 textOutput("cluster_predcr"),
                 textOutput("cluster_predpfs"),
                 plotOutput("plotpfs"),
                 textOutput("cluster_predos"),
                 plotOutput("plotos")
        ),
        tabPanel("Background and References",
                 h2("Background"),
                 br(),
                 h4("Chimeric antigen receptor T-cell (CAR-T) therapy represents a paradigm shift in the treatment of large B-cell lymphoma (LBCL), and other non-Hodgkin lymphomas (NHL)[1-8]. CAR-T treatment failure remains a significant challenge in LBCL, with nearly 50% of patients developing disease relapse or progression within the first 6 months[9-11]. Early risk stratification for CAR-T failure is of clinical interest."),
                 br(),
                 h4("Among studied correlates of CAR-T efficacy and toxicity, laboratory and cytokine measures of systemic inflammation stand out as widely available, and promising prognostic biomarkers[12-18]. InflaMix builds on this link between inflammation and poor CAR-T outcomes."),
                 br(),
                 h2("References"),
                 tags$ol(
                   tags$li("Neelapu, S.S., et al. Axicabtagene Ciloleucel CAR T-Cell Therapy in Refractory Large B-Cell Lymphoma. N Engl J Med 377, 2531-2544 (2017)."),
                   tags$li("Schuster, S.J., et al. Tisagenlecleucel in Adult Relapsed or Refractory Diffuse Large B-Cell Lymphoma. N Engl J Med 380, 45-56 (2019)."),
                   tags$li("Abramson, J.S., et al. Lisocabtagene maraleucel for patients with relapsed or refractory large B-cell lymphomas (TRANSCEND NHL 001): a multicentre seamless design study. The Lancet 396, 839-852 (2020)."),
                   tags$li("Locke, F.L., et al. Long-term safety and activity of axicabtagene ciloleucel in refractory large B-cell lymphoma (ZUMA-1): a single-arm, multicentre, phase 1-2 trial. Lancet Oncol 20, 31-42 (2019)."),
                   tags$li("5.	Locke, F.L., et al. Axicabtagene Ciloleucel as Second-Line Therapy for Large B-Cell Lymphoma. N Engl J Med 386, 640-654 (2022)."),
                   tags$li("Kamdar, M., et al. Lisocabtagene maraleucel versus standard of care with salvage chemotherapy followed by autologous stem cell transplantation as second-line treatment in patients with relapsed or refractory large B-cell lymphoma (TRANSFORM): results from an interim analysis of an open-label, randomised, phase 3 trial. Lancet 399, 2294-2308 (2022)."),
                   tags$li("Jacobson, C.A., et al. Axicabtagene ciloleucel in relapsed or refractory indolent non-Hodgkin lymphoma (ZUMA-5): a single-arm, multicentre, phase 2 trial. Lancet Oncol 23, 91-103 (2022)."),
                   tags$li("Wang, M., et al. KTE-X19 CAR T-Cell Therapy in Relapsed or Refractory Mantle-Cell Lymphoma. N Engl J Med 382, 1331-1342 (2020)."),
                   tags$li("Nastoupil, L.J., et al. Standard-of-Care Axicabtagene Ciloleucel for Relapsed or Refractory Large B-Cell Lymphoma: Results From the US Lymphoma CAR T Consortium. J Clin Oncol 38, 3119-3128 (2020)."),
                   tags$li("Alarcon Tomas, A., et al. Outcomes of first therapy after CD19-CAR-T treatment failure in large B-cell lymphoma. Leukemia 37, 154-163 (2023)."),
                   tags$li("Spiegel, J.Y., et al. Outcomes of patients with large B-cell lymphoma progressing after axicabtagene ciloleucel therapy. Blood 137, 1832-1835 (2021)."),
                   tags$li("Locke, F.L., et al. Tumor burden, inflammation, and product attributes determine outcomes of axicabtagene ciloleucel in large B-cell lymphoma. Blood Adv 4, 4898-4911 (2020)."),
                   tags$li("Vercellino, L., et al. Predictive factors of early progression after CAR T-cell therapy in relapsed/refractory diffuse large B-cell lymphoma. Blood Adv 4, 5607-5615 (2020)."),
                   tags$li("Pennisi, M., et al. Modified EASIX predicts severe cytokine release syndrome and neurotoxicity after chimeric antigen receptor T cells. Blood Adv 5, 3397-3406 (2021)."),
                   tags$li("Jagannath, S., et al. Tumor burden assessment and its implication for a prognostic model in advanced diffuse large-cell lymphoma. J Clin Oncol 4, 859-865 (1986)."),
                   tags$li("Ferraris, A.M., Giuntini, P. & Gaetani, G.F. Serum lactic dehydrogenase as a prognostic tool for non-Hodgkin lymphomas. Blood 54, 928-932 (1979)."),
                   tags$li("Jain, M.D., et al. Tumor interferon signaling and suppressive myeloid cells are associated with CAR T-cell failure in large B-cell lymphoma. Blood 137, 2621-2633 (2021)."),
                   tags$li("Rejeski, K., et al. CAR-HEMATOTOX: a model for CAR T-cell-related hematologic toxicity in relapsed/refractory large B-cell lymphoma. Blood 138, 2499-2513 (2021)."),
                   tags$li("Scrucca, L., Fop, M., Murphy, T.B. & Raftery, A.E. mclust 5: Clustering, Classification and Density Estimation Using Gaussian Finite Mixture Models. R J 8, 289-317 (2016)."),
                   tags$li("Website title created with BioRender.com")
                 )),
        tabPanel("Evidence",
                 h4("InflaMix is a risk stratification model for patients with large B-cell lymphoma receiving chimeric antigen receptor (CAR)-T cell therapy."),
                 br(),
                 h4("It calculates the probability that a patient carries an inflammatory biomarker signature before CAR-T infusion."),
                 br(),
                 h4("This signature is defined by elevated inflammatory markers (e.g., CRP, IL-6, ferritin), increased cell turnover (LDH), and end-organ sequelae of inflammation (e.g., hepatitis, bone marrow suppression), all assessed within the 2 days prior to CAR-T infusion."),
                 br(),
                 h4("It was derived by a Gaussian mixture model[19] of 14 pre-infusion lab measures and identified two clusters in a cohort of 149 patients at Memorial Sloan Kettering Cancer Center – an “inflammatory” cluster and a “non-inflammatory” cluster."),
                 br(),
                 h4("Patients in inflammatory cluster had reduced survival and disease response compared to patients in the non-inflammatory cluster even after accounting for other aggressive disease features including increased baseline tumor burden, primary refractory disease, age, and costimulatory domain of the CAR-T product."),
                 img(src = "deriv_otcm.png", height = 250, width = 680),
                 br(),
                 br(),
                 h4("These findings were validated in three independent cohorts of patients with non-Hodgkin lymphoma (including mantle cell and follicular lymphoma) across three treatment centers and inflammatory cluster assignment was predictive of disease relapse, progression, or death."),
                 br(),
                 h4("The associated paper is available at https://www.nature.com/articles/s41591-025-03532-x."),
                 br(),
                 h4("Terms of Use for InflaMix are detailed at https://github.com/vdblab/InflaMix/blob/main/LICENSE. Briefly, InflaMix is intended for educational and research use only and is not yet intended for clinical use or as a medical device.")
        ),
      )
      )
  )
)



load("model.RData")
# Define server logic required to draw a histogram ----
server <- function(input, output) {

  assign_clust <- function(x, mu, sig, pro){
    misfeats <- colnames(x)[!c(colnames(x) %in% rownames(as.data.frame(t(x)) %>% filter(!is.na(.))))]
    x1 <- x %>% select(!contains(misfeats))
    mu1 <- mu[!(rownames(mu) %in% misfeats),]
    sig1 <- sig[!(rownames(sig) %in% misfeats),!(rownames(sig) %in% misfeats),]
    cnum <- length(pro)
    densfun <- 1:cnum
    if(sum(dim(x1))==2){
      mu1 <- as.matrix(data.frame(t(mu1)))
      colnames(mu1) <- NULL
      rownames(mu1) <- colnames(x)[!c(colnames(x) %in% misfeats)]
      sig1 <- array(sig1, dim=c(1,1,2))
      colnames(sig1) <- colnames(x)[!c(colnames(x) %in% misfeats)]
      rownames(sig1) <- colnames(x)[!c(colnames(x) %in% misfeats)]
      return(NaN)
    }
    for(i in 1:cnum){
      densfun[i] <- mvtnorm::dmvnorm(x1, mean = mu1[,i], sigma = sig1[,,i], log = FALSE)
    }
    return( (pro * densfun) / sum(pro * densfun) )
  }

  shiny_assign <- function(crp, crpuln,
                           ldh, ldhuln,
                           hgb, hgbuln,
                           alp, alpuln,
                           ast, astuln,
                           ferritin, ferruln,
                           albumin, albuminuln,
                           plt, pltuln,
                           wbc, wbculn,
                           tbili, tbiliuln,
                           il6, il6uln,
                           il10, il10uln,
                           tnfa, tnfauln,
                           ddimer, ddimeruln){
    pt_dat <- data.frame(
      d0_albumin=((albumin/albuminuln) - as.numeric(sc_devcoh_d0[sc_devcoh_d0$lab=="albumin", 2]))/as.numeric(sc_devcoh_d0[sc_devcoh_d0$lab=="albumin", 3]),
      d0_alk=((alp/alpuln) - as.numeric(sc_devcoh_d0[sc_devcoh_d0$lab=="alk", 2]))/as.numeric(sc_devcoh_d0[sc_devcoh_d0$lab=="alk", 3]),
      d0_ast=((ast/astuln) - as.numeric(sc_devcoh_d0[sc_devcoh_d0$lab=="ast", 2]))/as.numeric(sc_devcoh_d0[sc_devcoh_d0$lab=="ast", 3]),
      d0_ferritin=((log(ferritin, 10)- log(ferruln, 10)) - as.numeric(sc_devcoh_d0[sc_devcoh_d0$lab=="ferritin", 2]))/as.numeric(sc_devcoh_d0[sc_devcoh_d0$lab=="ferritin", 3]),
      d0_hb=((hgb/hgbuln) - as.numeric(sc_devcoh_d0[sc_devcoh_d0$lab=="hb", 2]))/as.numeric(sc_devcoh_d0[sc_devcoh_d0$lab=="hb", 3]),
      d0_ldh=((ldh/ldhuln) - as.numeric(sc_devcoh_d0[sc_devcoh_d0$lab=="ldh", 2]))/as.numeric(sc_devcoh_d0[sc_devcoh_d0$lab=="ldh", 3]),
      d0_plt=((plt/pltuln) - as.numeric(sc_devcoh_d0[sc_devcoh_d0$lab=="plt", 2]))/as.numeric(sc_devcoh_d0[sc_devcoh_d0$lab=="plt", 3]),
      d0_tbr=((tbili/tbiliuln) - as.numeric(sc_devcoh_d0[sc_devcoh_d0$lab=="tbr", 2]))/as.numeric(sc_devcoh_d0[sc_devcoh_d0$lab=="tbr", 3]),
      d0_il10=((il10/il10uln) - as.numeric(sc_devcoh_d0[sc_devcoh_d0$lab=="il10", 2]))/as.numeric(sc_devcoh_d0[sc_devcoh_d0$lab=="il10", 3]),
      d0_il6=((il6/il6uln) - as.numeric(sc_devcoh_d0[sc_devcoh_d0$lab=="il6", 2]))/as.numeric(sc_devcoh_d0[sc_devcoh_d0$lab=="il6", 3]),
      d0_tnfa=((tnfa/tnfauln) - as.numeric(sc_devcoh_d0[sc_devcoh_d0$lab=="tnfa", 2]))/as.numeric(sc_devcoh_d0[sc_devcoh_d0$lab=="tnfa", 3]),
      d0_crp=((log(crp, 10) - log(crpuln, 10)) - as.numeric(sc_devcoh_d0[sc_devcoh_d0$lab=="crp", 2]))/as.numeric(sc_devcoh_d0[sc_devcoh_d0$lab=="crp", 3]),
      d0_ddimer=((ddimer/ddimeruln) - as.numeric(sc_devcoh_d0[sc_devcoh_d0$lab=="ddimer", 2]))/as.numeric(sc_devcoh_d0[sc_devcoh_d0$lab=="ddimer", 3]),
      d0_wbc=((wbc/wbculn) - as.numeric(sc_devcoh_d0[sc_devcoh_d0$lab=="wbc", 2]))/as.numeric(sc_devcoh_d0[sc_devcoh_d0$lab=="wbc", 3])
    )
    infclusprob <- assign_clust(x=pt_dat, mu=dev_mu, sig=dev_sig, pro=dev_pro)[dev_inflammClus]
    otpt <- c(
      ifelse(is.na(infclusprob) | is.nan(infclusprob), "Error", ifelse(infclusprob>0.5, "Inflammatory", "Non-Inflammatory")),
      ifelse(is.na(infclusprob) | is.nan(infclusprob), "Error", round(assign_clust(pt_dat, dev_mu, dev_sig, dev_pro)[dev_inflammClus], 2))
    )
    return(otpt)
  }

  output$cluster <- renderImage({
    req(
      (
        isTruthy(input$crp)+
        isTruthy(input$ldh)+
        isTruthy(input$hgb)+
        isTruthy(input$alp)+
        isTruthy(input$ast)+
        isTruthy(input$ferritin)+
        isTruthy(input$albumin)+
        isTruthy(input$plt)+
        isTruthy(input$tbili)+
        isTruthy(input$il6)+
        isTruthy(input$il10)+
        isTruthy(input$tnfa)+
        isTruthy(input$ddimer)+
        isTruthy(input$wbc)
      ) > 1
    )
    if(shiny_assign(crp = input$crp, crpuln=input$crpuln,
                           ldh = input$ldh, ldhuln = input$ldhuln,
                           hgb = input$hgb, input$hgbuln,
                           alp = input$alp, alpuln = input$alpuln,
                           ast = input$ast, astuln = input$astuln,
                           ferritin = input$ferritin, ferruln = input$ferruln,
                           albumin = input$albumin, albuminuln = input$albuminuln,
                           plt = input$plt, pltuln = input$pltuln,
                           wbc = input$wbc, wbculn = input$wbculn,
                           tbili = input$tbili, tbiliuln = input$tbiliuln,
                           il6 = input$il6, il6uln = input$il6uln,
                           il10 = input$il10, il10uln = input$il10,
                           tnfa = input$tnfa, tnfauln = input$tnfauln,
                           ddimer = input$ddimer, ddimeruln = input$ddimeruln)[1]=="Error") {
      ima <- "www/noclustext.png"} else{
        if(shiny_assign(crp = input$crp, crpuln=input$crpuln,
                        ldh = input$ldh, ldhuln = input$ldhuln,
                        hgb = input$hgb, input$hgbuln,
                        alp = input$alp, alpuln = input$alpuln,
                        ast = input$ast, astuln = input$astuln,
                        ferritin = input$ferritin, ferruln = input$ferruln,
                        albumin = input$albumin, albuminuln = input$albuminuln,
                        plt = input$plt, pltuln = input$pltuln,
                        wbc = input$wbc, wbculn = input$wbculn,
                        tbili = input$tbili, tbiliuln = input$tbiliuln,
                        il6 = input$il6, il6uln = input$il6uln,
                        il10 = input$il10, il10uln = input$il10,
                        tnfa = input$tnfa, tnfauln = input$tnfauln,
                        ddimer = input$ddimer, ddimeruln = input$ddimeruln)[1]=="Inflammatory") {
          ima <- "www/inflammtext.png"
        } else {ima <- "www/noninflammtext.png"}
      }
    list(src=ima, height=45)
  },     deleteFile = FALSE)

  output$clusterprob <- renderText({
    req(
      (
        isTruthy(input$crp)+
          isTruthy(input$ldh)+
          isTruthy(input$hgb)+
          isTruthy(input$alp)+
          isTruthy(input$ast)+
          isTruthy(input$ferritin)+
          isTruthy(input$albumin)+
          isTruthy(input$plt)+
          isTruthy(input$tbili)+
          isTruthy(input$il6)+
          isTruthy(input$il10)+
          isTruthy(input$tnfa)+
          isTruthy(input$ddimer)+
          isTruthy(input$wbc)
      ) > 1
    )
    t1 <- as.numeric(shiny_assign(crp = input$crp, crpuln=input$crpuln,
                                  ldh = input$ldh, ldhuln = input$ldhuln,
                                  hgb = input$hgb, input$hgbuln,
                                  alp = input$alp, alpuln = input$alpuln,
                                  ast = input$ast, astuln = input$astuln,
                                  ferritin = input$ferritin, ferruln = input$ferruln,
                                  albumin = input$albumin, albuminuln = input$albuminuln,
                                  plt = input$plt, pltuln = input$pltuln,
                                  wbc = input$wbc, wbculn = input$wbculn,
                                  tbili = input$tbili, tbiliuln = input$tbiliuln,
                                  il6 = input$il6, il6uln = input$il6uln,
                                  il10 = input$il10, il10uln = input$il10,
                                  tnfa = input$tnfa, tnfauln = input$tnfauln,
                                  ddimer = input$ddimer, ddimeruln = input$ddimeruln)[2])
    if(is.numeric(t1)) {
      paste0("Probability of Inflammatory Cluster: ", round(t1,2), "\n")
    } else {
      ""
    }

  })
}

shinyApp(ui = ui, server = server)

##############################################################################
######## ANN Training Script with embedding ##################################
##############################################################################



# Declare different inputs for separate embeddings
inputs <- list(
  input_actvsubs <- layer_input(shape = 6), 
  input_adults <- layer_input(shape = 6), 
  input_area <- layer_input(shape = 20), 
  input_crclscod <- layer_input(shape = 54), 
  input_creditcd <- layer_input(shape = 3), 
  input_dwllsize <- layer_input(shape = 16), 
  input_dwlltype <- layer_input(shape = 3), 
  input_ethnic <- layer_input(shape = 18), 
  input_forgntvl <- layer_input(shape = 3), 
  input_HHstatin <- layer_input(shape = 7), 
  input_income <- layer_input(shape = 10), 
  input_infobase <- layer_input(shape = 3), 
  input_kid0_2 <- layer_input(shape = 3), 
  input_kid11_15 <- layer_input(shape = 3), 
  input_kid16_17 <- layer_input(shape = 3), 
  input_kid3_5 <- layer_input(shape = 3), 
  input_kid6_10 <- layer_input(shape = 3), 
  input_lor <- layer_input(shape = 5), 
  input_marital <- layer_input(shape = 6), 
  input_new_cell <- layer_input(shape = 3), 
  input_numbcars <- layer_input(shape = 4), 
  input_ownrent <- layer_input(shape = 3), 
  input_prizm_social_one <- layer_input(shape = 6), 
  input_rv <- layer_input(shape = 3), 
  input_truck <- layer_input(shape = 3), 
  input_uniqsubs <- layer_input(shape = 5), 
  input_asl_flag <- layer_input(shape = 1),  #binary factor -> not one-hot-encoded
  input_dualband <- layer_input(shape = 4), 
  input_eqpdays <- layer_input(shape = 1), 
  input_hnd_price <- layer_input(shape = 5), 
  input_hnd_webcap <- layer_input(shape = 4), 
  input_models <- layer_input(shape = 5), 
  input_months <- layer_input(shape = 1), 
  input_mou_Mean <- layer_input(shape = 1), 
  input_phones <- layer_input(shape = 5), 
  input_refurb <- layer_input(shape = 1),  #binary factor -> not one-hot-encoded
  input_rev_Mean <- layer_input(shape = 1), 
  input_totmrc_Mean <- layer_input(shape = 1), 
  input_adjmou <- layer_input(shape = 1), 
  input_adjqty <- layer_input(shape = 1), 
  input_adjrev <- layer_input(shape = 1), 
  input_attempt_Mean <- layer_input(shape = 1), 
  input_avg3mou <- layer_input(shape = 1), 
  input_avg3qty <- layer_input(shape = 1), 
  input_avg3rev <- layer_input(shape = 1), 
  input_avg6mou <- layer_input(shape = 1), 
  input_avg6qty <- layer_input(shape = 1), 
  input_avg6rev <- layer_input(shape = 1), 
  input_avgmou <- layer_input(shape = 1), 
  input_avgqty <- layer_input(shape = 1), 
  input_avgrev <- layer_input(shape = 1), 
  input_blck_dat_Mean <- layer_input(shape = 1), 
  input_blck_vce_Mean <- layer_input(shape = 1), 
  input_callfwdv_Mean <- layer_input(shape = 1), 
  input_callwait_Mean <- layer_input(shape = 1), 
  input_cc_mou_Mean <- layer_input(shape = 1), 
  input_ccrndmou_Mean <- layer_input(shape = 1), 
  input_change_mou <- layer_input(shape = 1), 
  input_change_rev <- layer_input(shape = 1), 
  input_comp_dat_Mean <- layer_input(shape = 1), 
  input_comp_vce_Mean <- layer_input(shape = 1), 
  input_complete_Mean <- layer_input(shape = 1), 
  input_custcare_Mean <- layer_input(shape = 1), 
  input_da_Mean <- layer_input(shape = 1), 
  input_datovr_Mean <- layer_input(shape = 1), 
  input_drop_blk_Mean <- layer_input(shape = 1), 
  input_drop_dat_Mean <- layer_input(shape = 1), 
  input_drop_vce_Mean <- layer_input(shape = 1), 
  input_inonemin_Mean <- layer_input(shape = 1), 
  input_iwylis_vce_Mean <- layer_input(shape = 1), 
  input_mou_cdat_Mean <- layer_input(shape = 1), 
  input_mou_cvce_Mean <- layer_input(shape = 1), 
  input_mou_opkd_Mean <- layer_input(shape = 1), 
  input_mou_opkv_Mean <- layer_input(shape = 1), 
  input_mou_pead_Mean <- layer_input(shape = 1), 
  input_mou_peav_Mean <- layer_input(shape = 1), 
  input_mou_rvce_Mean <- layer_input(shape = 1), 
  input_mouiwylisv_Mean <- layer_input(shape = 1), 
  input_mouowylisv_Mean <- layer_input(shape = 1), 
  input_opk_dat_Mean <- layer_input(shape = 1), 
  input_opk_vce_Mean <- layer_input(shape = 1), 
  input_ovrmou_Mean <- layer_input(shape = 1), 
  input_ovrrev_Mean <- layer_input(shape = 1), 
  input_owylis_vce_Mean <- layer_input(shape = 1), 
  input_peak_dat_Mean <- layer_input(shape = 1), 
  input_peak_vce_Mean <- layer_input(shape = 1), 
  input_plcd_dat_Mean <- layer_input(shape = 1), 
  input_plcd_vce_Mean <- layer_input(shape = 1), 
  input_recv_sms_Mean <- layer_input(shape = 1), 
  input_recv_vce_Mean <- layer_input(shape = 1), 
  input_roam_Mean <- layer_input(shape = 1), 
  input_threeway_Mean <- layer_input(shape = 1), 
  input_totcalls <- layer_input(shape = 1), 
  input_totmou <- layer_input(shape = 1), 
  input_totrev <- layer_input(shape = 1), 
  input_unan_dat_Mean <- layer_input(shape = 1), 
  input_unan_vce_Mean <- layer_input(shape = 1), 
  input_vceovr_Mean <- layer_input(shape = 1)
)



# Concatination Layer
concat <- layer_concatenate(
  list(
    input_actvsubs %>% layer_embedding(input_dim = 7, output_dim = as.integer(ceiling(nlevels(telco$actvsubs)**0.25)*3), name = "actvsubs") %>% layer_flatten(), 
    input_adults %>% layer_embedding(input_dim = 7, output_dim = as.integer(ceiling(nlevels(telco$adults)**0.25)*3), name = "adults") %>% layer_flatten(), 
    input_area %>% layer_embedding(input_dim = 21, output_dim = as.integer(ceiling(nlevels(telco$area)**0.25)*3), name = "area") %>% layer_flatten(), 
    input_crclscod %>% layer_embedding(input_dim = 55, output_dim = as.integer(ceiling(nlevels(telco$crclscod)**0.25)*3), name = "crclscod") %>% layer_flatten(), 
    input_creditcd %>% layer_embedding(input_dim = 4, output_dim = as.integer(ceiling(nlevels(telco$creditcd)**0.25)*3), name = "creditcd") %>% layer_flatten(), 
    input_dwllsize %>% layer_embedding(input_dim = 17, output_dim = as.integer(ceiling(nlevels(telco$dwllsize)**0.25)*3), name = "dwllsize") %>% layer_flatten(), 
    input_dwlltype %>% layer_embedding(input_dim = 4, output_dim = as.integer(ceiling(nlevels(telco$dwlltype)**0.25)*3), name = "dwlltype") %>% layer_flatten(), 
    input_ethnic %>% layer_embedding(input_dim = 19, output_dim = as.integer(ceiling(nlevels(telco$ethnic)**0.25)*3), name = "ethnic") %>% layer_flatten(), 
    input_forgntvl %>% layer_embedding(input_dim = 4, output_dim = as.integer(ceiling(nlevels(telco$forgntvl)**0.25)*3), name = "forgntvl") %>% layer_flatten(), 
    input_HHstatin %>% layer_embedding(input_dim = 8, output_dim = as.integer(ceiling(nlevels(telco$HHstatin)**0.25)*3), name = "HHstatin") %>% layer_flatten(), 
    input_income %>% layer_embedding(input_dim = 11, output_dim = as.integer(ceiling(nlevels(telco$income)**0.25)*3), name = "income") %>% layer_flatten(), 
    input_infobase %>% layer_embedding(input_dim = 4, output_dim = as.integer(ceiling(nlevels(telco$infobase)**0.25)*3), name = "infobase") %>% layer_flatten(), 
    input_kid0_2 %>% layer_embedding(input_dim = 4, output_dim = as.integer(ceiling(nlevels(telco$kid0_2)**0.25)*3), name = "kid0_2") %>% layer_flatten(), 
    input_kid11_15 %>% layer_embedding(input_dim = 4, output_dim = as.integer(ceiling(nlevels(telco$kid11_15)**0.25)*3), name = "kid11_15") %>% layer_flatten(), 
    input_kid16_17 %>% layer_embedding(input_dim = 4, output_dim = as.integer(ceiling(nlevels(telco$kid16_17)**0.25)*3), name = "kid16_17") %>% layer_flatten(), 
    input_kid3_5 %>% layer_embedding(input_dim = 4, output_dim = as.integer(ceiling(nlevels(telco$kid3_5)**0.25)*3), name = "kid3_5") %>% layer_flatten(), 
    input_kid6_10 %>% layer_embedding(input_dim = 4, output_dim = as.integer(ceiling(nlevels(telco$kid6_10)**0.25)*3), name = "kid6_10") %>% layer_flatten(), 
    input_lor %>% layer_embedding(input_dim = 6, output_dim = as.integer(ceiling(nlevels(telco$lor)**0.25)*3), name = "lor") %>% layer_flatten(), 
    input_marital %>% layer_embedding(input_dim = 7, output_dim = as.integer(ceiling(nlevels(telco$marital)**0.25)*3), name = "marital") %>% layer_flatten(), 
    input_new_cell %>% layer_embedding(input_dim = 4, output_dim = as.integer(ceiling(nlevels(telco$new_cell)**0.25)*3), name = "new_cell") %>% layer_flatten(), 
    input_numbcars %>% layer_embedding(input_dim = 5, output_dim = as.integer(ceiling(nlevels(telco$numbcars)**0.25)*3), name = "numbcars") %>% layer_flatten(), 
    input_ownrent %>% layer_embedding(input_dim = 4, output_dim = as.integer(ceiling(nlevels(telco$ownrent)**0.25)*3), name = "ownrent") %>% layer_flatten(), 
    input_prizm_social_one %>% layer_embedding(input_dim = 7, output_dim = as.integer(ceiling(nlevels(telco$prizm_social_one)**0.25)*3), name = "prizm_social_one") %>% layer_flatten(), 
    input_rv %>% layer_embedding(input_dim = 4, output_dim = as.integer(ceiling(nlevels(telco$rv)**0.25)*3), name = "rv") %>% layer_flatten(), 
    input_truck %>% layer_embedding(input_dim = 4, output_dim = as.integer(ceiling(nlevels(telco$truck)**0.25)*3), name = "truck") %>% layer_flatten(), 
    input_uniqsubs %>% layer_embedding(input_dim = 6, output_dim = as.integer(ceiling(nlevels(telco$uniqsubs)**0.25)*3), name = "uniqsubs") %>% layer_flatten(), 
    input_asl_flag, 
    input_dualband %>% layer_embedding(input_dim = 5, output_dim = as.integer(ceiling(nlevels(telco$dualband)**0.25)*3), name = "dualband") %>% layer_flatten(), 
    input_eqpdays, 
    input_hnd_price %>% layer_embedding(input_dim = 6, output_dim = as.integer(ceiling(nlevels(telco$hnd_price)**0.25)*3), name = "hnd_price") %>% layer_flatten(), 
    input_hnd_webcap %>% layer_embedding(input_dim = 5, output_dim = as.integer(ceiling(nlevels(telco$hnd_webcap)**0.25)*3), name = "hnd_webcap") %>% layer_flatten(), 
    input_models %>% layer_embedding(input_dim = 6, output_dim = as.integer(ceiling(nlevels(telco$models)**0.25)*3), name = "models") %>% layer_flatten(), 
    input_months, 
    input_mou_Mean, 
    input_phones %>% layer_embedding(input_dim = 6, output_dim = as.integer(ceiling(nlevels(telco$phones)**0.25)*3), name = "phones") %>% layer_flatten(), 
    input_refurb, 
    input_rev_Mean, 
    input_totmrc_Mean, 
    input_adjmou, 
    input_adjqty, 
    input_adjrev, 
    input_attempt_Mean, 
    input_avg3mou, 
    input_avg3qty, 
    input_avg3rev, 
    input_avg6mou, 
    input_avg6qty, 
    input_avg6rev, 
    input_avgmou, 
    input_avgqty, 
    input_avgrev, 
    input_blck_dat_Mean, 
    input_blck_vce_Mean, 
    input_callfwdv_Mean, 
    input_callwait_Mean, 
    input_cc_mou_Mean, 
    input_ccrndmou_Mean, 
    input_change_mou, 
    input_change_rev, 
    input_comp_dat_Mean, 
    input_comp_vce_Mean, 
    input_complete_Mean, 
    input_custcare_Mean, 
    input_da_Mean, 
    input_datovr_Mean, 
    input_drop_blk_Mean, 
    input_drop_dat_Mean, 
    input_drop_vce_Mean, 
    input_inonemin_Mean, 
    input_iwylis_vce_Mean, 
    input_mou_cdat_Mean, 
    input_mou_cvce_Mean, 
    input_mou_opkd_Mean, 
    input_mou_opkv_Mean, 
    input_mou_pead_Mean, 
    input_mou_peav_Mean, 
    input_mou_rvce_Mean, 
    input_mouiwylisv_Mean, 
    input_mouowylisv_Mean, 
    input_opk_dat_Mean, 
    input_opk_vce_Mean, 
    input_ovrmou_Mean, 
    input_ovrrev_Mean, 
    input_owylis_vce_Mean, 
    input_peak_dat_Mean, 
    input_peak_vce_Mean, 
    input_plcd_dat_Mean, 
    input_plcd_vce_Mean, 
    input_recv_sms_Mean, 
    input_recv_vce_Mean, 
    input_roam_Mean, 
    input_threeway_Mean, 
    input_totcalls, 
    input_totmou, 
    input_totrev, 
    input_unan_dat_Mean, 
    input_unan_vce_Mean, 
    input_vceovr_Mean
  )
)


##############################################################################
######## Hyperparameter flags ################################################
##############################################################################
# flags have to be part of this training script to be shown in the result table

FLAGS <- flags(
  flag_integer("nHL", 5),  #Nr. of layers in network
  flag_integer("nnodes", 32),  #Nr. of nodes for hidden layer 1
  flag_string("actHL", 'relu'),  #activation function for hidden layer 1
  flag_numeric("droprate", 0.1),
  flag_string("actOL", 'softmax'),  #activation function for output layer
  flag_numeric("lr", 0.001),  #learning rate
  flag_string("comp_loss", 'binary_crossentropy'),  #compiler: loss function
  flag_string("comp_metric", 'accuracy'),  #compiler: metric
  flag_integer("epochsMax", 400),  #Nr. of epochs max.
  flag_integer("batch_size", 128),  #batch size
  flag_string("earlyStop_monitor", "val_loss"),
  flag_numeric("earlyStop_min_delta", 0),
  flag_integer("earlyStop_patience", 50),
  flag_boolean("restore_best_weights", TRUE)
)



# Output layer
output <- 
  concat %>%
  layer_dense(units = FLAGS$nnodes, activation = FLAGS$actHL) %>%  # Hidden Layer 1
  layer_dropout(rate = FLAGS$droprate) %>%
  layer_dense(units = FLAGS$nnodes, activation = FLAGS$actHL) %>%  # Hidden Layer 2
  layer_dropout(rate = FLAGS$droprate) %>%
  layer_dense(units = FLAGS$nnodes, activation = FLAGS$actHL) %>%  # Hidden Layer 3
  layer_dropout(rate = FLAGS$droprate) %>%
  layer_dense(units = FLAGS$nnodes, activation = FLAGS$actHL) %>%  # Hidden Layer 4
  layer_dropout(rate = FLAGS$droprate) %>%
  layer_dense(units = FLAGS$nnodes, activation = FLAGS$actHL) %>%  # Hidden Layer 5
  layer_dropout(rate = FLAGS$droprate) %>%
  layer_dense(units = 2, activation = FLAGS$actOL)


# Initialize model
model <- NULL
model <- keras_model(inputs, output)

# Compile the model
model %>% compile(
  loss = FLAGS$comp_loss,
  optimizer = optimizer_adam(lr = FLAGS$lr),
  metrics = FLAGS$comp_metric
)

# Create callbacks (e.g. early stopping)
callbacks <- list(
  callback_early_stopping(monitor = FLAGS$earlyStop_monitor, min_delta = FLAGS$earlyStop_min_delta, 
                          patience = FLAGS$earlyStop_patience, restore_best_weights = FLAGS$restore_best_weights)
)

# Fit the model 
history <- model %>% fit(
  train.dummy.X, 
  train.dummy.Y, 
  epochs = FLAGS$epochsMax, 
  batch_size = FLAGS$batch_size, 
  validation_split = 0.1,
  callbacks = callbacks
)




# Evaluate on train data and labels
score <- model %>% evaluate(train.dummy.X, train.dummy.Y, batch_size = 128)
score$loss
score$acc
# Evaluate on test data and labels
score <- model %>% evaluate(test.dummy.X, test.dummy.Y, batch_size = 128)
score$loss
score$acc                

            
plot(history)


cat('Test loss:', score$loss, '\n')
cat('Test accuracy:', score$acc, '\n')
              
   



jeunes_train = read.csv("jeunes_essais/jeunes_train.csv", row.names = 1)
jeunes_test = read.csv("jeunes_essais/jeunes_test.csv", row.names = 1)

jeunes_train$diff_age = as.numeric(as.character(jeunes_train$diff_age))
jeunes_test$diff_age = as.numeric(as.character(jeunes_test$diff_age))

darty_train = read.csv("Darty_essais/darty_test.csv", row.names = 1)
darty_test = read.csv("Darty_essais/darty_train.csv", row.names = 1)

bale2_train = read.csv("bale2_essais/bale2_train.csv", na.strings = ".")
bale2_test = read.csv("bale2_essais/bale2_test.csv", na.strings = ".")

jeunes_train = subset(jeunes_train, select = -c(C_CANAL_DIST,NATION,SITFAM))
glm_jeunes = glm(top_pic_perf12 ~ ., data=jeunes_train,family=binomial(link="logit"))
glmdisc::normalizedGini(jeunes_test$top_pic_perf12,predict(glm_jeunes,jeunes_test,type="response"))

darty_train$CSP = factor(darty_train$CSP)
darty_test$CSP = factor(darty_test$CSP)
darty_train$CSPCJ = factor(darty_train$CSPCJ)
darty_test$CSPCJ = factor(darty_test$CSPCJ)

darty_train$CSP_cat = factor(darty_train$CSP_cat)
darty_test$CSPCJ_cat = factor(darty_test$CSPCJ_cat)
darty_train$CSPCJ_cat = factor(darty_train$CSPCJ_cat)
darty_test$CSP_cat = factor(darty_test$CSP_cat)
darty_test$SITFAM_cat = factor(darty_test$SITFAM_cat)
darty_train$SITFAM_cat = factor(darty_train$SITFAM_cat)

darty_train = subset(darty_train, select = -c(SITFAM,SITFAM_cat,CSP_cat,CSPCJ_cat,HABIT_cat))

glm_darty = glm(TOP_PIC ~ ., data=darty_train,family=binomial(link="logit"))
glmdisc::normalizedGini(darty_test$TOP_PIC,predict(glm_darty,darty_test,type="response"))


glm_bale2 = glm(X13 ~ ., data=bale2_train,family=binomial(link="logit"))
glmdisc::normalizedGini(bale2_test$X13,predict(glm_bale2,bale2_test,type="response"))

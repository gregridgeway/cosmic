functions {

  real logDenomDP_logspace(vector lambda, vector s, array[] int y, int nOff) {
    int m = nOff;
    int J = num_elements(s);

    matrix[m, J] logP;
    real logK = 0;

    for (i in 1:m) {
      vector[J] eta;
      for (j in 1:J)
        eta[j] = lambda[i] * s[j];

      real log_norm = log_sum_exp(eta);

      for (j in 1:J)
        logP[i, j] = eta[j] - log_norm;

      logK += log_norm;
    }

    array[J] int k;
    for (j in 1:J) k[j] = 0;
    for (i in 1:m) k[y[i]] += 1;

    if (J == 1) return logK;

    int d = J - 1;

    array[d] int dims;
    int total_non_J = 0;
    int total_states = 1;

    for (j in 1:d) {
      dims[j] = k[j] + 1;
      total_non_J += k[j];
      total_states *= dims[j];
    }

    array[d] int strides;
    strides[1] = 1;
    for (j in 2:d)
      strides[j] = strides[j-1] * dims[j-1];

    vector[total_states] log_dp_old;
    vector[total_states] log_dp_new;

    log_dp_old = rep_vector(negative_infinity(), total_states);
    log_dp_old[1] = 0;

    int kJ = k[J];

    array[d] int u;

    for (r in 1:m) {

      log_dp_new = rep_vector(negative_infinity(), total_states);

      int lo = max(0, (r-1) - kJ);
      int hi = min(r-1, total_non_J);

      for (j in 1:d) u[j] = 0;
      int sum_u = 0;

      for (idx in 1:total_states) {

        real val = log_dp_old[idx];

        if (sum_u >= lo && sum_u <= hi) {

          // category J
          log_dp_new[idx] =
            log_sum_exp(log_dp_new[idx],
                        val + logP[r, J]);

          // categories 1..J-1
          for (j in 1:d) {
            if (u[j] < k[j]) {
              int new_idx = idx + strides[j];
              log_dp_new[new_idx] =
                log_sum_exp(log_dp_new[new_idx],
                            val + logP[r, j]);
            }
          }
        }

        // odometer
        for (j in 1:d) {
          u[j] += 1;
          sum_u += 1;

          if (u[j] < dims[j])
            break;

          sum_u -= u[j];
          u[j] = 0;
        }
      }

      log_dp_old = log_dp_new;
    }

    int target_idx = 1;
    for (j in 1:d)
      target_idx += k[j] * strides[j];

    return logK + log_dp_old[target_idx];
  } // end logDenomDP_logspace


  // for computing cond log likelihood in parallel
  real partial_sum(array[] int iaIndexStart,
                   int iStart, int iEnd,
                   array[] int ivY,
                   array[] int ivID,
                   array[] int ivOffID,
                   vector rvLambda,
                   vector rvSTemp,
                   int nRows,
                   int nMaxOffs)
  {
    int  i=0;
    int  iCurrentID = 0;
    int  nOff = 0;
    real rLogLL = 0.0;
    real rNumerator = 0.0;

    array[nMaxOffs] int ivYTemp;
    vector[nMaxOffs]    rvLambdaTemp;

    // get first row in dataset for incident iStart
    i = iaIndexStart[1];

    while((i<=nRows) && (ivID[i] <= iEnd))
    {
      nOff = 0; // count of officers in this incident
      rNumerator = 0.0;
      iCurrentID = ivID[i];
      while((i<=nRows) && (ivID[i] == iCurrentID)) // still the same incident?
      {
         // cond log likelihood numerator
         rNumerator += rvLambda[ivOffID[i]]*rvSTemp[ivY[i]];

         // accumulate data for current incident
         nOff = nOff+1;
         ivYTemp[nOff] = ivY[i];
         rvLambdaTemp[nOff] = rvLambda[ivOffID[i]];
         i = i+1;
      }

      if (nOff == 2)
      {
        if (ivYTemp[1] != ivYTemp[2])
        {
          real a = rvLambdaTemp[1] * rvSTemp[ivYTemp[1]]
                 + rvLambdaTemp[2] * rvSTemp[ivYTemp[2]];

          real b = rvLambdaTemp[1] * rvSTemp[ivYTemp[2]]
                 + rvLambdaTemp[2] * rvSTemp[ivYTemp[1]];

          rLogLL += rNumerator - log_sum_exp(a, b);
        }
        // else: identical y -> contributes 0
      }
      else {
        rLogLL += rNumerator -
           logDenomDP_logspace(rvLambdaTemp, rvSTemp, ivYTemp, nOff);
      }
    }

    return rLogLL;
  } // end partial_sum()


  // compute log conditional likelihood of full dataset
  real logCL(array[] int ivY,
             array[] int ivID,
             array[] int ivOffID,
             array[] int ivStartIndex,
             vector rvLambda,
             vector rvSDelta, // not including 1st two, fixed s[1:2]={0,1}
             int nRows,
             int nMaxOffs,
             int nForceTypes,
             int grainsize)
  {
    vector[nForceTypes] rvSTemp;
    real rLogLL = 0.0;

    rvSTemp[1] = 0.0;
    rvSTemp[2] = 1.0;
    for(k in 3:nForceTypes)
    {
      rvSTemp[k] = rvSTemp[k-1] + rvSDelta[k-2];
    }

    // compute log LL in parallel by incident
    rLogLL = reduce_sum(partial_sum, ivStartIndex,
                        grainsize,
                        ivY,
                        ivID,
                        ivOffID,
                        rvLambda,
                        rvSTemp,
                        nRows,
                        nMaxOffs);

    return rLogLL;
  } // end logCL()

} // end functions


data {
  int<lower=0> nMaxOffs;      // largest number of officers in an incident
                              //   for allocating space
  int<lower=0> nRows;         // all officer*incident count
  int<lower=0> nOff;          // number of unique officers in dataset
  int<lower=0> nIncidents;    // number of incidents
  int<lower=0> nForceTypes;   // number of different force types
  array[nRows] int y;               // force category: 1, 2, ..., nForceTypes
  array[nRows] int id;              // incident ID: 1, 2, ..., nIncidents
  array[nRows] int idOff;           // officer ID: 1, 2, ..., nOff
  array[nIncidents] int startIndex; // startIndex[id] is the row number containing
                              //   location of first officer for incident id
                              //   for speeding up parallel comp of logCL

  real<lower=0> rPriorSD_lambda;
  real<lower=0> rPriorSD_sDiff;
}


parameters {
  vector[nOff] lambda;            // officer-specific propensity to escalate force
  vector<lower=0>[nForceTypes-2] sDelta; // stereotype model parameter
                                  // s[1]=0, s[2]=1 for identifiability
                                  // s[3]=s[2]+sDelta[1], etc
}

model {
  for(i in 1:(nForceTypes-2))
  {
     sDelta[i] ~ normal(0, rPriorSD_sDiff); // half-normal, sDelta is <lower=0>
  }
  for(i in 1:nOff)
  {
     lambda[i] ~ normal(0, rPriorSD_lambda); // put some constraints on lambda
  }

  // conditional likelihood term
  target += logCL(y, id, idOff, startIndex,
                  lambda, sDelta, nRows, nMaxOffs, nForceTypes,
                  1);  // grainsize
}

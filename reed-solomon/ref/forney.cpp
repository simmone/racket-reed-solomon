 *   - Forney's algorithm states that the error values for a Reed-solomon code
 *     are computed by:
 *     e_ik = omega(X_k^-1)/derivative[lambda(X_k^-1)] where X_k^-1 is a root of
 *            lambda(x) and i is the index of the error location
 *
 * Assumptions:
 *   - all arrays are setup and have memory allocated
 *   - the err_loc array is of size n, and is set at indices where the
 *     rc_x has errors
 *   - lambda and omega arrays are of size 2*t + 1
 *  
 * Arguments:
 *   - lambda = lambda(x);  error locator polynomial
 *   - omega = omega(x); error magnitude/value/evaluator polynomial
 *   - err_loc = array that states if a given polynomial coeff.
 *               is an error location
 *
 * Return:
 *   None
 *
 * Operation:
 *   - Get derivative of lambda(x) = lambda(x)'
 *   - Using this determine decoded codeword, dc(x) with algorithm:
 *     + FOR all ccodeword indexed by i FROM 0 to (n-1)
 *         if (at error location) dc_i = rc_i + e
 *         else                   dc_i = rc_i
 *
 * Revision History
 *   Jun 01, 2011    Nnoduka Eruchalu    Initial Revision
 *   Mar 16, 2014    Nnoduka Eruchalu    Cleaned up comments
 */
void reedSolomon::forney(int *lambda, int *omega, int *err_loc)
{
    int size = 2*t+1;
    
    // lambdap(x) = derivative[lambda(x)] = lambda(x)'
    int * lambdap = new int[size];  
    for(int i=0; i < size-1; i++)
        lambdap[i] = ((i+1)%2)*lambda[i+1];
    lambdap[size-1] = 0;
    
    // setup decoded word and fix it
    for(int i=0; i < n; i++)
    {
        if(err_loc[i])
        {
            int num = 0;   // numerator
            int denum = 0; // denominator
            for(int j=0; j< size; j++)
            {
                if(omega[j])
                    num ^= alpha_to[ (index_of[omega[j]]+ ((n-i)%n)*j)%n ];
                if(lambdap[j])
                    denum ^= alpha_to[ (index_of[lambdap[j]]+ ((n-i)%n)*j)%n ];
            }
            dc_x[i] = rc_x[i]^alpha_to[(index_of[num] - index_of[denum] + n)%n];
        }
        else // err_loc[i] == 0  // rc(x) is correct at this index
            dc_x[i] = rc_x[i];
    }
    
    delete [] lambdap;
}

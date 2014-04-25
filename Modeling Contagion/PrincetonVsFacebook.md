Three weeks ago, researchers at Princeton released a study on Epidemiological modeling of online social network dynamics that states Facebook might lose 80% of its users by 2015-2017. Facebook data scientists hilariously debunked the study stating that Princeton itself would lose all of its students by 2021, using the same methodology used in the original study. The media joined the hype, with TechCrunch, CNN, Mashable, CNET etc offering their comments on the study and methodology.

The reality is neither of these institutions are going out of business. Last year Princeton admitted 7.4% of the applicants, still maintaining the status of a premier educational institution. Facebook has over 1.2 billion active users.  However, Facebook has lower engagement from teens and faces threat from a number of new social networks. The article from Times of India explores various possibilities for the social network.  Similarly, traditional universities are threatened by the rise of online education platforms such as edx, Coursera and Udacity. One of the key assumptions behind using SIR model is that there are ‘waves of social networks’ (eg. Classmates.com, Friendster, MySpace) that come and go. It may also happen that there would be growth and stability instead of growth and decline.

Here is the Google search trends for Myspace:

MySpace

and here is the Google trends plot for Facebook:

FB

However, our interest is to use R as a platform in modeling contagion. These are useful in modeling propagation of diseases, marketing message, viral videos, etc.  The simplest and classic model is the SIR model which considers fixed population with only three compartments: susceptible, infected and removed. The compartments used for this model consist of three classes:

S(t) is used to represent the number of individuals not yet infected with the disease at time t, or those susceptible to the disease.
I(t) denotes the number of individuals who have been infected with the disease and are capable of spreading the disease to those in the susceptible category.
R(t) is the compartment used for those individuals who have been infected and then removed from the disease, either due to immunization or due to death.
If N is the fixed population, N = S(t) + I(t) + R(t).

SIR

Let’s assume that an individual in the population has an equal probability as every other individual of contracting the disease with a rate of ‘beta’ (infection rate of the disease).Also assume that a number equal to the fraction ‘gamma’ (recovery rate) , infected are entering the removed class

We derive the following differential equations:

dS/dt = - beta * S * I
dI/dt = beta*S*I - gamma*I
dR/dt = gamma*I

I used  “deSolve” package to model this and “optim” function to optimize the solution. Here is what I got:

Myspace Model

If you would like to improve this model, here is the code. If you would like to explore other models for modeling contagion, see Download SIR Model v3. Let us know if you have any interesting findings.
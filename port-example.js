// Example JavaScript port implementation for Onboarding plan type
// This would typically be added to your JavaScript initialization file
// where you set up Elm ports

// Initialize the Elm app and connect ports
const app = Elm.Main.init({
  node: document.getElementById('elm-app'),
  flags: {
    // Any flags your app needs
  }
});

// Handle retrieving plan type from localStorage or API
app.ports.retrievePlanType.subscribe(function(orgSlug) {
  // In a real app, this would check localStorage or make an API call
  const planType = localStorage.getItem('plan_type_' + orgSlug) || 'basic';
  
  console.log('Retrieved plan type:', planType, 'for org:', orgSlug);
  
  // Send the plan type back to Elm
  app.ports.planTypeReceived.send(planType);
});

// Handle saving and retrieving user details
app.ports.storeUserDetails.subscribe(function(userData) {
  console.log('Storing user details for:', userData.organizationId);
  localStorage.setItem('user_details_' + userData.organizationId, JSON.stringify(userData));
});

app.ports.retrieveUserDetails.subscribe(function(organizationId) {
  console.log('Retrieving user details for:', organizationId);
  const userDetailsStr = localStorage.getItem('user_details_' + organizationId);
  const userData = userDetailsStr ? JSON.parse(userDetailsStr) : null;
  
  // Send back only the user data fields needed by the UserDetails module
  if (userData) {
    app.ports.userDetailsReceived.send({
      firstName: userData.firstName,
      lastName: userData.lastName,
      email: userData.email,
      phone: userData.phone
    });
  } else {
    app.ports.userDetailsReceived.send(null);
  }
});

// Storage port for when a plan is selected
// You would need to add this port to your Elm app
app.ports.storePlanType.subscribe(function(data) {
  console.log('Storing plan type:', data.planType, 'for org:', data.orgSlug);
  localStorage.setItem('plan_type_' + data.orgSlug, data.planType);
});

// Other port handlers for existing functionality
app.ports.storeOnboardingState.subscribe(function(state) {
  console.log('Storing onboarding state:', state);
  localStorage.setItem('onboarding_state', JSON.stringify(state));
});

app.ports.retrieveOnboardingState.subscribe(function() {
  const stateStr = localStorage.getItem('onboarding_state');
  const state = stateStr ? JSON.parse(stateStr) : null;
  console.log('Retrieved onboarding state:', state);
  app.ports.onboardingStateReceived.send(state);
});

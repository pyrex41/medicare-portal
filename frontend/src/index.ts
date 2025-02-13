case '/settings/add-agents':
  app = Elm.AddAgent.init({
    flags: {
      isSetup: isSetup,
      key: key,
    }
  });
  break; 
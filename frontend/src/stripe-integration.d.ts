export function loadStripeScript(): void;

export interface StripeResult {
  success: boolean;
  error?: string;
}

export interface StripeIntegration {
  initializeStripe(publishableKey: string): boolean;
  redirectToCheckout(sessionId: string): Promise<StripeResult>;
  processPayment(clientSecret: string): Promise<StripeResult>;
  cleanupStripe(): void;
}

export const stripeIntegration: StripeIntegration;

declare global {
  interface Window {
    Stripe: any;
    stripeIntegration: StripeIntegration;
  }
} 
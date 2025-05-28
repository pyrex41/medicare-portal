#!/bin/bash

# Usage: ./test_billing_endpoints.sh <session_cookie>
SESSION_COOKIE="${1:-5e296135324bad158796b97d1cec9bece2b7f3dae33314570969b459081d37fc}"
BASE_URL="http://localhost:5173"

echo "=== /api/billing/plan ==="
curl -s --cookie "session=$SESSION_COOKIE" "$BASE_URL/api/billing/plan" | jq
echo

echo "=== /api/billing/payment-method ==="
curl -s --cookie "session=$SESSION_COOKIE" "$BASE_URL/api/billing/payment-method" | jq
echo

echo "=== /api/billing/invoices ==="
curl -s --cookie "session=$SESSION_COOKIE" "$BASE_URL/api/billing/invoices" | jq
echo 
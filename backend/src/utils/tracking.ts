/**
 * Utilities for tracking URLs and link analytics
 */

/**
 * Generates a tracking ID for a particular URL
 * Format: tid-[orgId]-[contactId]-[timestamp]
 * @param orgId Organization ID
 * @param contactId Contact ID
 * @param prefix Optional prefix for the tracking ID (defaults to 'tid')
 * @returns Tracking ID string
 */
export const generateTrackingId = (orgId: number | string, contactId: number | string, prefix: string = 'tid'): string => {
  const timestamp = Date.now();
  return `${prefix}-${orgId}-${contactId}-${timestamp}`;
};

/**
 * Adds tracking parameters to a URL
 * @param url The original URL
 * @param trackingId The tracking ID to add
 * @returns URL with tracking parameters
 */
export const addTrackingToUrl = (url: string, trackingId: string): string => {
  try {
    const urlObj = new URL(url);
    urlObj.searchParams.append('tid', trackingId);
    return urlObj.toString();
  } catch (error) {
    // If URL parsing fails, append as a simple query parameter
    const separator = url.includes('?') ? '&' : '?';
    return `${url}${separator}tid=${trackingId}`;
  }
};

/**
 * Extracts information from a tracking ID
 * @param trackingId The tracking ID to parse
 * @returns Object with parsed tracking info or null if invalid
 */
export const parseTrackingId = (trackingId: string): { prefix: string, orgId: string, contactId: string, timestamp: number } | null => {
  const parts = trackingId.split('-');
  if (parts.length < 4) {
    return null;
  }
  
  const [prefix, orgId, contactId, timestampStr] = parts;
  const timestamp = parseInt(timestampStr, 10);
  
  if (isNaN(timestamp)) {
    return null;
  }
  
  return {
    prefix,
    orgId,
    contactId,
    timestamp
  };
}; 
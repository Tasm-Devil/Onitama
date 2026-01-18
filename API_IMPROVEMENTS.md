# API Improvement Proposals

**Date:** 2026-01-18
**Current API Version:** `/1/onitama/games/*`

## Completion Status

**Phase 1 Complete** ✅ (2026-01-18)
- ✅ Path parameters instead of query params
- ✅ Proper HTTP status codes
- ✅ Consistent error handling across all endpoints
- ✅ RESTful resource hierarchy
- ✅ HTTP method semantics (PUT → POST for join)
- ✅ CORS support (already implemented, updated for new header)

The API now follows REST best practices with proper resource hierarchy, structured error types, and standard HTTP semantics.

---

## My decisions on the Remaining Enhancements

- 1. Keep it as it is. There is no need to change it for now.
- 2. Thought about it. This is maybe a good idea because I maybe want OAuth later. But for now, this is overkill for this project.
- 3. Haven't thought about it but could become relevant, when this software is actually used.
- 4. Absolute Overkill
- 5. Usefull but very low priority

## Remaining Enhancements

### 1. API Path Naming Philosophy

**Priority:** 🟡 MEDIUM (Decision needed)
**Affected Files:** `server/src/Api.hs`, documentation

**Current Situation:**
- Path is `/1/onitama/games/...`
- CLAUDE.md states "game-agnostic server" philosophy
- Hardcoding "onitama" contradicts game-agnostic design

**Option A: Game-Agnostic Paths**
```
/1/games/{id}
/1/games/{id}/moves
```
- Pros: Truly reusable for any turn-based game
- Cons: Might host other games later (naming conflicts?)

**Option B: Keep Current (Game-Specific API)**
```
/1/onitama/games/{id}
/1/onitama/games/{id}/moves
```
- Pros: Clear namespace, allows multiple games on same server
- Cons: More verbose, contradicts stated philosophy if only hosting Onitama

**Option C: Hybrid (Game Type Parameter)**
```
/1/games/{gameType}/{id}  → /1/games/onitama/5
```
- Pros: Supports multiple games, keeps server generic
- Cons: More complex routing

**Recommendation:**
- Keep **Option B** if you plan to add other games
- Switch to **Option A** if only hosting Onitama
- Update CLAUDE.md to reflect actual philosophy

**Impact:**
- Philosophical/documentation clarity
- Breaking change if switching from B to A
- No technical blocker, just design decision

**Estimated Effort:** Low (30 minutes for code change if switching to A, 5 minutes for docs update only)

---

### 2. Improve Authentication Header Naming

**Priority:** 🟢 LOW (Nice-to-have)
**Affected Files:** `server/src/Api.hs`, `server/app/Main.hs`, `client/src/Api.elm`

**Current:**
```haskell
Header "X-Session-Token" SessionToken
```

**Proposed:**
```haskell
-- Option A: Standard Authorization header
Header "Authorization" AuthHeader

-- Where client sends:
-- Authorization: Bearer <token>

-- Option B: Keep custom header but follow convention
Header "X-Onitama-Session-Token" SessionToken
```

**Pros of Standard Header:**
- Works with standard auth libraries
- Better for API gateways/proxies
- Industry standard

**Cons:**
- Not technically OAuth/Bearer (might be misleading)
- Current approach works fine and is clear

**Recommendation:**
Keep current approach unless integrating with OAuth/standard auth later.

**Impact:**
- Low priority
- Breaking change if implemented
- No functional benefit, just convention

**Estimated Effort:** Low (1-2 hours)

---

### 3. Add Pagination for Game List

**Priority:** 🟢 LOW (Future-proofing)
**Affected Files:** `server/src/Api.hs`, `server/src/App.hs`, `server/src/Database.hs`

**Current:**
```haskell
type GetGameSummaries = "1" :> "onitama" :> "games" :> Get '[JSON] [GameSummary]
```

**Proposed:**
```haskell
type GetGameSummaries = "1" :> "onitama" :> "games"
                     :> QueryParam "page" Int
                     :> QueryParam "perPage" Int
                     :> Get '[JSON] PaginatedGames

data PaginatedGames = PaginatedGames
  { pgGames :: [GameSummary]
  , pgTotal :: Int
  , pgPage :: Int
  , pgPerPage :: Int
  , pgHasMore :: Bool
  }
  deriving (Generic, ToJSON, FromJSON)
```

**Alternative (Headers):**
Use standard `Link` headers like GitHub API:
```
Link: <https://api.example.com/games?page=2>; rel="next"
X-Total-Count: 150
```

**Benefits:**
- Scales to many games
- Reduces bandwidth
- Standard practice

**Current Need:**
Low (unlikely to have thousands of games soon)

**Impact:**
- Breaking change if replacing current endpoint
- Non-breaking if adding new endpoint alongside existing

**Estimated Effort:** Medium (2-3 hours)

---

### 4. Add HATEOAS Links

**Priority:** 🟢 LOW (Advanced REST)
**Affected Files:** `server/src/Api.hs`

**Current:**
Clients must construct URLs themselves.

**Proposed:**
```haskell
data GameWithNames = GameWithNames
  { gameWhiteName :: Text
  , gameBlackName :: Text
  , gameCards :: [Card]
  , gameHistory :: [GameMove]
  , gameWinner :: Maybe Color
  , gameLinks :: GameLinks  -- NEW
  }

data GameLinks = GameLinks
  { linkSelf :: Text       -- /1/onitama/games/5
  , linkMoves :: Text      -- /1/onitama/games/5/moves
  , linkConcede :: Maybe Text  -- Only if player can concede
  }
```

**Benefits:**
- Self-documenting API
- Clients don't hardcode URL construction
- Enables API evolution without breaking clients

**Complexity:**
- Overkill for simple API
- Adds response payload size
- More complex server code

**Recommendation:**
Skip unless building public API or expecting many API consumers.

**Estimated Effort:** Medium (3-4 hours)

---

### 5. Add OpenAPI/Swagger Documentation

**Priority:** 🟢 LOW
**Affected Files:** New `server/swagger.yaml` or use `servant-swagger`

**Proposed:**
```haskell
-- Add dependency: servant-swagger
import Servant.Swagger

apiDocs :: Swagger
apiDocs = toSwagger api
  & info.title .~ "Onitama API"
  & info.version .~ "1.0"
  & info.description ?~ "Multiplayer Onitama game server"

-- Serve at /swagger.json
type APIWithDocs = API :<|> "swagger.json" :> Get '[JSON] Swagger
```

**Benefits:**
- Auto-generated documentation
- Can generate client libraries
- Interactive API explorer (Swagger UI)

**Impact:**
- No breaking changes
- Adds build complexity
- Very useful for onboarding developers

**Estimated Effort:** Medium (2-3 hours)

---

## Implementation Roadmap

### ✅ Phase 1: Critical Fixes - COMPLETE (2026-01-18)
- Path parameters instead of query params
- Proper HTTP status codes (404, structured errors)
- Consistent error handling (MoveError, ConcedeError types)
- RESTful resource hierarchy (`/games/{id}/moves`)
- HTTP method semantics (PUT → POST for join)
- CORS support (updated for X-Session-Token)

**Result:** Production-ready REST API with proper error handling and standard semantics.

---

### Phase 2: Optional Enhancements

**Tasks:**
1. **Issue #1:** Decide on path naming philosophy (5-30 minutes)
2. **Issue #5:** Add OpenAPI/Swagger docs (2-3 hours)
3. **Issue #3:** Add pagination when needed (2-3 hours)

**Current Recommendation:**
- Update documentation to clarify naming philosophy (Issue #1)
- Defer Issues #2, 3, 4, 5 until actually needed

---

## Decision Matrix

| Issue | Priority | Breaking? | Effort | Current Need |
|-------|----------|-----------|--------|--------------|
| #1 Naming philosophy | MEDIUM | Maybe | Low | Docs clarity |
| #2 Auth header | LOW | Yes | Low | None |
| #3 Pagination | LOW | Yes | Medium | Low (few games) |
| #4 HATEOAS | LOW | No | Medium | None |
| #5 Swagger docs | LOW | No | Medium | Medium (nice-to-have) |

---

## Recommendation

**Immediate Action:**
- Update CLAUDE.md and README.md to clarify if server is game-agnostic or game-specific (Issue #1)

**Quick Wins:**
- All Phase 1 items already complete ✅
- CORS already implemented and updated ✅

**Can Defer:**
- Issues #2-5: Wait until actually needed
- Only implement Swagger docs (Issue #5) if expecting external developers or want interactive API explorer

---

## Summary

Phase 1 is complete! The API now has:
- ✅ RESTful resource hierarchy
- ✅ Path parameters for resource identification
- ✅ Proper HTTP status codes (404, etc.)
- ✅ Structured error types for all operations
- ✅ Correct HTTP methods (POST for creating relationships)
- ✅ CORS support with correct headers

All remaining items are optional enhancements that can be implemented as needed.

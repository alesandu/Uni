// --- Auth UI: Login, Register, Session Management ---

// Mappa ruolo → pagina
const ROLE_PAGES = {
    'LAB': 'lab.html',
    'TRANSPORT_LIGHT': 'van.html',
    'HUB': 'hub.html',
    'TRANSPORT_HAZ': 'truck.html',
    'LANDFILL': 'landfill.html',
    'OPERATOR': 'history.html',
    'SUPERUSER': 'alerts.html'
};

const _currentPath = window.location.pathname;
const _isLoginPage = _currentPath.endsWith('login.html') || _currentPath.endsWith('login-only.html') || _currentPath === '/';

// Verifica la sessione con il server (cookie JWT) — fonte di verità
(async function checkSession() {
    let serverUser = null;
    try {
        serverUser = await API.me();
    } catch (_) {
        // Sessione non valida o assente
        serverUser = null;
    }

    if (!serverUser) {
        // Sessione non valida: pulisci localStorage e vai al login se necessario
        localStorage.removeItem('user');
        if (!_isLoginPage) {
            window.location.href = 'login-only.html';
        }
        return;
    }

    // Sessione valida: sincronizza localStorage col server
    localStorage.setItem('user', JSON.stringify(serverUser));
    const currentUser = serverUser;

    // --- Access Control: pagine riservate al SUPERUSER ---
    const isSuperuserPage = _currentPath.endsWith('logs.html') || _currentPath.endsWith('alerts.html') || _currentPath.endsWith('admin.html');
    if (isSuperuserPage && currentUser.role !== 'SUPERUSER') {
        window.location.href = ROLE_PAGES[currentUser.role] || 'login-only.html';
        return;
    }

    // Se siamo sulla pagina di login ma la sessione è valida → redirect alla pagina del ruolo
    if (_isLoginPage) {
        window.location.href = ROLE_PAGES[currentUser.role] || 'lab.html';
        return;
    }

    // --- Aggiornamento UI navbar ---
    // Dopo un await, DOMContentLoaded potrebbe essere già scattato:
    // eseguiamo subito la callback se il DOM è già pronto.
    function onReady(fn) {
        if (document.readyState === 'loading') {
            window.addEventListener('DOMContentLoaded', fn, { once: true });
        } else {
            fn();
        }
    }

    onReady(() => {
        // Navbar dinamica: nascondi/mostra link in base al ruolo
        const navLinks = document.querySelector('.nav-links');
        if (navLinks) {
            if (currentUser.role !== 'SUPERUSER') {
                navLinks.querySelectorAll('a').forEach(a => {
                    const href = a.getAttribute('href');
                    if (href === 'alerts.html' || href === 'logs.html' || href === 'admin.html') {
                        a.remove();
                    }
                });
            } else {
                const existingAdmin = navLinks.querySelector('a[href="admin.html"]');
                if (!existingAdmin) {
                    const adminLink = document.createElement('a');
                    adminLink.href = 'admin.html';
                    adminLink.textContent = 'Admin';
                    if (_currentPath.endsWith('admin.html')) adminLink.classList.add('active');
                    navLinks.appendChild(adminLink);
                }
            }
        }

        const authDiv = document.querySelector('.nav-auth');
        if (authDiv) {
            authDiv.innerHTML = `<button id="btn-logout" class="btn text" style="color: white;"> Logout (${currentUser.username})</button>`;
            document.getElementById('btn-logout').addEventListener('click', async () => {
                try {
                    await API.logout();
                } catch (e) {
                    // silently handled
                }
                localStorage.removeItem('user');
                window.location.href = 'login-only.html';
            });
        }
    });
})();


// --- Login Page Functions ---

function switchTab(tab) {
    try {
        document.getElementById('tab-login').classList.remove('active');
        document.getElementById('tab-register').classList.remove('active');
        document.getElementById('form-login').classList.remove('active');
        document.getElementById('form-register').classList.remove('active');

        document.getElementById(`tab-${tab}`).classList.add('active');
        document.getElementById(`form-${tab}`).classList.add('active');

        clearMessages();
    } catch (e) {
        // silently handled
    }
}

function clearMessages() {
    const errEl = document.getElementById('error-msg');
    const sucEl = document.getElementById('success-msg');
    if (errEl) errEl.style.display = 'none';
    if (sucEl) sucEl.style.display = 'none';
}

function showError(msg) {
    const el = document.getElementById('error-msg');
    if (!el) return;
    el.textContent = msg;
    el.style.display = 'block';
    const sucEl = document.getElementById('success-msg');
    if (sucEl) sucEl.style.display = 'none';
}

function showSuccess(msg) {
    const el = document.getElementById('success-msg');
    if (!el) return;
    el.textContent = msg;
    el.style.display = 'block';
    const errEl = document.getElementById('error-msg');
    if (errEl) errEl.style.display = 'none';
}

// Login handler
async function handleLogin(e) {
    e.preventDefault();
    clearMessages();
    const btn = e.target.querySelector('button[type="submit"]');
    const originalText = btn.textContent;
    btn.textContent = 'Authenticating...';
    btn.disabled = true;

    const username = document.getElementById('login-username').value;
    const password = document.getElementById('login-password').value;

    try {
        const data = await API.login(username, password);

        localStorage.setItem('user', JSON.stringify(data.user));

        const rolePages = {
            'LAB': 'lab.html',
            'TRANSPORT_LIGHT': 'van.html',
            'HUB': 'hub.html',
            'TRANSPORT_HAZ': 'truck.html',
            'LANDFILL': 'landfill.html',
            'OPERATOR': 'lab.html',
            'SUPERUSER': 'alerts.html'
        };
        const targetPage = rolePages[data.user.role] || 'lab.html';

        btn.textContent = 'Access Granted. Redirecting...';
        btn.style.background = 'var(--primary)';

        setTimeout(() => {
            window.location.href = targetPage;
        }, 500);
    } catch (err) {
        showError(err.message || 'Login failed');
        btn.textContent = originalText;
        btn.disabled = false;
    }
}

// Register handler
async function handleRegister(e) {
    e.preventDefault();
    clearMessages();
    const btn = e.target.querySelector('button[type="submit"]');
    const originalText = btn.textContent;
    btn.textContent = 'Registering...';
    btn.disabled = true;

    const username = document.getElementById('reg-username').value;
    const password = document.getElementById('reg-password').value;
    const role = document.getElementById('reg-role').value;

    const regData = { username, password, role };

    try {
        await API.register(regData);

        showSuccess('Registration successful! You can now access the network.');
        document.getElementById('form-register').reset();
        btn.textContent = 'Success!';

        setTimeout(() => {
            switchTab('login');
            document.getElementById('login-username').value = username;
            document.getElementById('login-password').focus();
            btn.textContent = originalText;
            btn.disabled = false;
        }, 1000);
    } catch (err) {
        showError(err.message || 'Registration failed');
        btn.textContent = originalText;
        btn.disabled = false;
    }
}

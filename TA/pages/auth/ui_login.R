tabItemLogin <- tabItem(
  tabName = "loginpage",
  div(class = "center-box",
      box(
        title = "Login",
        width = 6,
        status = "primary",
        solidHeader = TRUE,
        class = "center-content",
        conditionalPanel(
          condition = "input.LOGINSTATUS == '0'",
          h6(tags$b("Username")),
          textInput(
            'username', "Username"
          ),
          h6(tags$b("Username")),
          textInput(
            'password', "Password"
          ),
          actionButton('loginBtn', "Login")
        ),
        conditionalPanel(
          condition = "input.LOGINSTATUS == '1'",
          h4(tags$b("Klik tombol dibawah untuk keluar")),
          actionButton('logoutBtn', "Logout")
        )
      )
  )
)